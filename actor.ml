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

  val return_task : 'a -> 'a task
  val enqueue_cast : 'data pid -> 'data T.cast -> (unit -> 'a task) -> 'a task
  val enqueue_call : 'data pid -> ('data, 'ret) T.call -> ('ret -> 'a task) -> 'a task

  val new_executor : unit -> t
  val add_task : t -> unit M.t -> unit
  val run_tasks : t -> unit
end = struct
  (* instancja aktora: wewnętrzny stan i lista obliczeń do wykonania *)
  type 'a pid = 'a ref * unit M.t Queue.t
  (* stan zadania do wykonania/wykonanego na executorze *)
  and 'a task =
  | SentTo of unit M.t Queue.t
  | Finished of 'a
  | Enqueued of (unit -> 'a task)
  (* stan executora: kolejka obliczeń do wykonania *)
  and t = unit M.t Queue.t

  let spawn (type data) (module M : Actor with type data = data) =
    ref (M.default ()), Queue.create ()

  let return_task x = Finished x

  let enqueue_cast (pid : 'data pid) (fn : 'data T.cast) (k : unit -> 'a task) : 'a task =
    Enqueued (fun () ->
      let data, queue = pid in
      let task : unit M.t =
        M.bind
          (M.flatten (M.return_lazy (fun () -> fn !data)))
          (fun new_data ->
            data := new_data;
            M.return ())
      in
      Queue.push task queue;
      k ())
  let enqueue_call (pid : 'data pid) (fn : ('data, 'ret) T.call) (k : 'ret -> 'a task) : 'a task =
    Enqueued (fun () ->
      let data, queue = pid in
      let task : unit M.t =
        M.bind
          (M.flatten (M.return_lazy (fun () -> fn !data)))
          (fun (new_data, ret) ->
            data := new_data;
            let _ = k ret in
            M.return ())
      in
      Queue.push task queue;
      SentTo queue)

  let new_executor () : t = Queue.create ()
  let add_task t task = Queue.push task t
  let rec run_task t task =
    match task with
    | SentTo queue ->
      Queue.push
        (M.return_lazy (fun () -> Queue.transfer queue t))
        t
    | Finished value -> ()
    | Enqueued fn -> run_task t (fn ())
  let run_tasks t =
    while not (Queue.is_empty t) do
      run_task t (Queue.pop t |> M.into_task)
    done
end

and M : sig
  type 'a t
  type 'data pid

  val return : 'a -> 'a t
  val return_lazy : (unit -> 'a) -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val flatten : 'a t t -> 'a t

  val spawn : (module Actor with type data = 'data) -> 'data pid
  val cast : 'data pid -> 'data T.cast -> unit t
  val call : 'data pid -> ('data, 'a) T.call -> 'a t
  val into_task : 'a t -> 'a Executor.task
end = struct
  type 'a t = { run : 'r. ('a -> 'r Executor.task) -> 'r Executor.task }
  type 'data pid = 'data Executor.pid

  let return x = { run = fun k -> k x }
  let return_lazy f = { run = fun k -> k (f ()) }
  let bind a f = { run = fun k -> a.run (fun x -> (f x).run k ) }
  let flatten x = bind x (fun y -> y)

  let spawn (type data) (module M : Actor with type data = data) =
    Executor.spawn (module M)
  let cast pid fn =
    { run = fun k -> Executor.enqueue_cast pid fn k }
  let call pid fn =
    { run = fun k -> Executor.enqueue_call pid fn k }
  let into_task t =
    t.run Executor.return_task
end

and T : sig
  type 'data cast = 'data -> 'data M.t
  type ('data, 'ret) call = 'data -> ('data * 'ret) M.t
end = struct
  type 'data cast = 'data -> 'data M.t
  type ('data, 'ret) call = 'data -> ('data * 'ret) M.t
end
