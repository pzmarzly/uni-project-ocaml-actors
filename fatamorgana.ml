module type Actor = sig
  type data
  val default : unit -> data
end

module rec Executor : sig
  type 'data pid
  type 'a task
  type t

  val spawn : (module Actor with type data = 'data) -> 'data pid
  val enqueue_cast : 'data pid -> 'data M.cast -> (unit -> unit task) -> unit task
  val enqueue_call : 'data pid -> ('data, 'ret) M.call -> ('ret -> unit task) -> unit task
  val wait_read : Unix.file_descr -> (unit -> unit task) -> unit task
  val wait_write : Unix.file_descr -> (unit -> unit task) -> unit task

  val create : unit -> t
  val add : t -> unit M.t -> unit
  val run_tasks : t -> unit
end = struct
  (* actor inbox is a list of tasks operating on its state *)
  type inbox = unit task list ref
  (* actor instance (process): state + inbox *)
  and 'a pid = 'a ref * inbox
  (* state of executor task *)
  and 'a task =
  | Finished of 'a
  | Lazy of (unit -> 'a task)
  | ProcessInbox of inbox
  | Pair of 'a task * 'a task (* now, later *)
  | WaitRead of Unix.file_descr * 'a task
  | WaitWrite of Unix.file_descr * 'a task
  (* state of executor: task queue, rd_tab, wr_tab *)
  and t =
    unit task Queue.t
    * (Unix.file_descr, unit task) Hashtbl.t
    * (Unix.file_descr, unit task) Hashtbl.t

  let spawn (type data) (module M : Actor with type data = data) =
    ref (M.default ()), ref []
  let enqueue_cast (pid : 'data pid) (fn : 'data M.cast) (k : unit -> unit task) : unit task =
    Lazy (fun () ->
      let data, inbox = pid in
      let m : unit M.t =
        M.bind
          (M.flatten (M.return_lazy (fun () -> fn !data)))
          (fun new_data ->
            data := new_data;
            M.return ()) in
      let t : unit task =
        Lazy (fun () -> M.into_task m (fun () -> Finished ())) in
      inbox := t :: !inbox;
      Pair (Lazy k, ProcessInbox inbox))
  let enqueue_call (pid : 'data pid) (fn : ('data, 'ret) M.call) (k : 'ret -> unit task) : unit task =
    Lazy (fun () ->
      let data, inbox = pid in
      let m : 'ret M.t =
        M.bind
          (M.flatten (M.return_lazy (fun () -> fn !data)))
          (fun (new_data, ret) ->
            data := new_data;
            M.return ret) in
      let t : unit task =
        Lazy (fun () -> M.into_task m k) in
      inbox := t :: !inbox;
      Pair (Finished (), ProcessInbox inbox))
  let wait_read (fd : Unix.file_descr) (k : unit -> unit task) : unit task =
    Lazy (fun () ->
      WaitRead (fd, Lazy k))
  let wait_write (fd : Unix.file_descr) (k : unit -> unit task) : unit task =
    Lazy (fun () ->
      WaitWrite (fd, Lazy k))

  let create () : t = Queue.create (), Hashtbl.create 32, Hashtbl.create 32
  let add_task (tasks, _, _) task = Queue.push task tasks
  let add exec m = add_task exec (M.into_task m (fun () -> Finished ()))
  let rec run_task_regular (tasks, rd, wr) task =
    match task with
    | ProcessInbox inbox ->
      let t = Lazy (fun () ->
        let inbox_state = !inbox in
        inbox := [];
        Queue.add_seq tasks (inbox_state |> List.rev |> List.to_seq);
        Finished ())
      in
      Queue.push t tasks
    | Finished value -> ()
    | Lazy fn -> run_task_regular (tasks, rd, wr) (fn ())
    | Pair (t1, t2) ->
      Queue.push t2 tasks;
      run_task_regular (tasks, rd, wr) t1
    | WaitRead (fd, k) -> Hashtbl.add rd fd k
    | WaitWrite (fd, k) -> Hashtbl.add wr fd k
  let run_tasks_regular (tasks, rd, wr) =
    let ran = ref false in
    while not (Queue.is_empty tasks) do
      run_task_regular (tasks, rd, wr) (Queue.pop tasks);
      ran := true
    done;
    !ran
  let exec_fds (tasks, rd, wr) (rd_fds, wr_fds) =
    List.iter (fun fd ->
      let task = Hashtbl.find rd fd in
      Hashtbl.remove rd fd;
      Queue.push task tasks) rd_fds;
    List.iter (fun fd ->
      let task = Hashtbl.find wr fd in
      Hashtbl.remove wr fd;
      Queue.push task tasks) wr_fds
  let run_tasks_io (tasks, rd, wr) =
    if Hashtbl.length rd > 0 || Hashtbl.length wr > 0
      then
        let rd_fds = Hashtbl.to_seq_keys rd |> List.of_seq in
        let wr_fds = Hashtbl.to_seq_keys wr |> List.of_seq in
        let (rd_fds, wr_fds, err_fds) = Unix.select rd_fds wr_fds [] (-1.0) in
        exec_fds (tasks, rd, wr) (rd_fds, wr_fds);
        true
      else false
  let rec run_tasks exec =
    if run_tasks_regular exec || run_tasks_io exec
      then run_tasks exec
      else ()
end

and M : sig
  type 'a t
  type 'data pid

  type 'data cast = 'data -> 'data M.t
  type ('data, 'ret) call = 'data -> ('data * 'ret) M.t

  val return : 'a -> 'a t
  val return_lazy : (unit -> 'a) -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val flatten : 'a t t -> 'a t

  val spawn : (module Actor with type data = 'data) -> 'data pid
  val cast : 'data pid -> 'data cast -> unit t
  val call : 'data pid -> ('data, 'a) call -> 'a t

  val wait_read : Unix.file_descr -> unit t
  val wait_write : Unix.file_descr -> unit t

  val into_task : 'a t -> ('a -> unit Executor.task) -> unit Executor.task
end = struct
  type 'a t = ('a -> unit Executor.task) -> unit Executor.task
  type 'data pid = 'data Executor.pid

  type 'data cast = 'data -> 'data M.t
  type ('data, 'ret) call = 'data -> ('data * 'ret) M.t

  let return x = fun k -> k x
  let return_lazy f = fun k -> k (f ())
  let bind a f = fun k -> a (fun x -> f x k )
  let flatten x = bind x (fun y -> y)

  let spawn (type data) (module M : Actor with type data = data) =
    Executor.spawn (module M)
  let cast (pid : 'data pid) (fn : 'data cast) : unit t =
    fun k -> Executor.enqueue_cast pid fn k
  let call (pid : 'data pid) (fn : ('data, 'a) call) : 'a t =
    fun k -> Executor.enqueue_call pid fn k
  let wait_read (fd : Unix.file_descr) : unit t =
    fun k -> Executor.wait_read fd k
  let wait_write (fd : Unix.file_descr) : unit t =
    fun k -> Executor.wait_write fd k

  let into_task t fn = t fn
end

type 'ret monadic = 'ret M.t
let return = M.return
let bind = M.bind
let (let*) = M.bind

type 'data pid = 'data M.pid
type 'data cast = 'data M.cast
type ('data, 'ret) call = ('data, 'ret) M.call
let spawn = M.spawn
let cast = M.cast
let call = M.call
let wait_read = M.wait_read
let wait_write = M.wait_write

let sleep ms =
  let fd = Timerfd.create Timerfd.Clock.monotonic ms in
  let* () = wait_read fd in
  Unix.close fd;
  return ()

module Yield : sig
  include Actor
  val noop : (data, unit) call
end = struct
  type data = unit
  let default () = ()
  let noop () = return ((), ())
end
let yield = call (spawn (module Yield)) Yield.noop
