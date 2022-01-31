type serializer =
| SBool
| SInt
| SList of serializer

module type Actor = sig
  type data
  val data_format : (string * serializer) list
  val default : unit -> data
end

module rec Executor : sig
  type 'data pid
  type 'a task
  type t

  val spawn : (module Actor with type data = 'data) -> 'data pid
  val enqueue_cast : 'data pid -> 'data M.cast -> (unit -> unit task) -> unit task
  val enqueue_call : 'data pid -> ('data, 'ret) M.call -> ('ret -> unit task) -> unit task

  val new_executor : unit -> t
  val add_task : t -> unit task -> unit
  val add : t -> unit M.t -> unit
  val run_tasks : t -> unit
end = struct
  (* message box *)
  type inbox = unit task list ref
  (* instancja aktora: wewnętrzny stan i lista obliczeń do wykonania *)
  and 'a pid = 'a ref * inbox
  (* stan zadania do wykonania/wykonanego na executorze *)
  and 'a task =
  | Finished of 'a
  | Enqueue of (unit -> 'a task)
  | ProcessInbox of inbox
  | Pair of 'a task * 'a task
  (* stan executora: kolejka obliczeń do wykonania *)
  and t = unit task Queue.t

  let spawn (type data) (module M : Actor with type data = data) =
    ref (M.default ()), ref []
  let enqueue_cast (pid : 'data pid) (fn : 'data M.cast) (k : unit -> unit task) : unit task =
    Enqueue (fun () ->
      let data, queue = pid in
      let m : unit M.t =
        M.bind
          (M.flatten (M.return_lazy (fun () -> fn !data)))
          (fun new_data ->
            data := new_data;
            M.return ()) in
      let t : unit task =
        Enqueue (fun () -> M.into_task m (fun () -> Finished ())) in
      queue := t :: !queue;
      Pair(k (), ProcessInbox queue))
  let enqueue_call (pid : 'data pid) (fn : ('data, 'ret) M.call) (k : 'ret -> unit task) : unit task =
    Enqueue (fun () ->
      let data, queue = pid in
      let m : 'ret M.t =
        M.bind
          (M.flatten (M.return_lazy (fun () -> fn !data)))
          (fun (new_data, ret) ->
            data := new_data;
            M.return ret) in
      let t : unit task =
        Enqueue (fun () -> M.into_task m (fun ret -> (Enqueue (fun () -> k ret)))) in
      queue := t :: !queue;
      ProcessInbox queue)

  let new_executor () : t = Queue.create ()
  let add_task exec task = Queue.push task exec
  let add exec m = add_task exec (M.into_task m (fun () -> Finished ()))
  let rec run_task exec task =
    match task with
    | ProcessInbox inbox ->
      let t = Enqueue (fun () ->
        let inbox_state = !inbox in
        inbox := [];
        inbox_state |> List.rev |> List.to_seq |> Queue.add_seq exec;
        Finished ())
      in
      Queue.push t exec
    | Finished value -> ()
    | Enqueue fn -> run_task exec (fn ())
    | Pair (t1, t2) ->
      Queue.push t2 exec;
      run_task exec t1
  let run_tasks exec =
    while not (Queue.is_empty exec) do
      run_task exec (Queue.pop exec)
    done
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
  let into_task t fn = t fn
end

type 'data pid = 'data M.pid
type 'data cast = 'data M.cast
type ('data, 'ret) call = ('data, 'ret) M.call
let return = M.return
let bind = M.bind
let spawn = M.spawn
let cast = M.cast
let call = M.call
