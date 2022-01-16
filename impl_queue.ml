open Actor

module Executor : sig
  type 'data pid
  val make_pid : (module Actor with type data = 'data) -> ('data pid -> 'a) -> 'a

  type 'a task
  val return_task : 'a -> 'a task
  val enqueue_call : 'data pid -> ('data, 'ret) call -> ('ret -> 'a task) -> 'a task

  type t
  val new_executor : unit -> t
  val add_task : t -> unit task -> unit
  val run_tasks : t -> unit
end = struct
  type 'a pid = 'a ref * (unit -> unit) list ref
  let make_pid (type data) (module M : Actor with type data = data) k =
    k (ref M.default, ref [])

  type 'a task =
  | SentTo of (unit -> unit) list ref
  | Finished of 'a
  | Enqueued of (unit -> 'a task)
  let return_task x = Finished x

  let enqueue_call pid fn k =
    Enqueued (fun () ->
      let data, queue = pid in
      let task = (fun () ->
        let new_data, ret = fn !data in
        data := new_data;
        let _ = k ret in ()
      ) in
      queue := task :: !queue;
      SentTo queue)

  type t = unit task Queue.t
  let new_executor = Queue.create
  let add_task t task = Queue.push task t
  let rec drain_queue_task queue =
    Enqueued (fun () ->
      match !queue with
      | [] -> Finished ()
      | x :: xs -> x (); queue := xs; drain_queue_task queue)
  let rec run_task t task =
    match task with
    | SentTo queue -> Queue.push (drain_queue_task queue) t
    | Finished value -> ()
    | Enqueued fn -> run_task t (fn ())
  let run_tasks t =
    while not (Queue.is_empty t) do
      run_task t (Queue.pop t)
    done
end

module QueueMonad : sig
  type 'a t
  type 'data pid

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val spawn : (module Actor with type data = 'data) -> 'data pid t
  val call : 'data pid -> ('data, 'a) call -> 'a t
  val into_task : 'a t -> 'a Executor.task
end = struct
  type 'a t = { run : 'r. ('a -> 'r Executor.task) -> 'r Executor.task }
  type 'data pid = 'data Executor.pid

  let return x = { run = fun k -> k x }
  let bind a f = { run = fun k -> a.run (fun x -> (f x).run k ) }

  let spawn (type data) (module M : Actor with type data = data) =
    { run = fun k -> Executor.make_pid (module M) k }

  let call pid fn =
    { run = fun k -> Executor.enqueue_call pid fn k }

  let into_task t =
    t.run Executor.return_task
end
