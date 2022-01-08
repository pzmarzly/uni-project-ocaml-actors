open Actor

module Executor : sig
  type 'data pid
  val make_pid : (module Actor with type data = 'data) -> ('data pid -> 'a) -> 'a

  type queue
  type 'a task =
  | Finished of 'a
  | Enqueued of (queue -> 'a task)

  val enqueue_call : 'data pid -> ('data, 'ret) call -> ('ret -> 'a task) -> 'a task
  val run : 'a task -> 'a
end = struct
  type 'a pid = 'a ref
  let make_pid (type data) (module M : Actor with type data = data) k =
    k (ref M.default)

  type queue = (unit -> unit) list
  type 'a task =
  | Finished of 'a
  | Enqueued of (queue -> 'a task)

  let enqueue_call pid fn k =
    Enqueued (fun queue ->
      let data, ret = fn !pid in
      pid := data;
      k ret)

  let rec run task =
    match task with
    | Finished value -> value
    | Enqueued fn -> run (fn [])
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
    t.run (fun x -> Finished x)
end
