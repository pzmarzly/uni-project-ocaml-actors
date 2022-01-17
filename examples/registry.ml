open Fatamorgana.Actor
open Fatamorgana.Actor.M
let (let* ) = bind

module Registry (Impl : Actor) : sig
  include (module type of Impl)
  val get : unit -> Impl.data M.pid
  val set : Impl.data M.pid -> unit
end = struct
  include Impl
  let instance = ref (M.spawn (module Impl))
  let get () = !instance
  let set v = instance := v
end

module Counter
: sig
  include Actor
  val increase : data cast
  val get : (data, int) call
  val set : int -> (data, int) call
end
= struct
  type data = int
  let data_format = [("value", SInt)]
  let default () = 0
  let increase v = M.return (v + 1)
  let get v = M.return (v, v)
  let set new_state state = M.return (new_state, state)
end

module CounterInstance = Registry(Counter)

let main1 =
  let* () = cast (CounterInstance.get ()) Counter.increase in
  let* current = call (CounterInstance.get ()) Counter.get in
  return (Printf.printf "%i\n" current)

let main2 =
  let* () = cast (CounterInstance.get ()) Counter.increase in
  let* current = call (CounterInstance.get ()) Counter.get in
  return (Printf.printf "%i\n" current)

let _ =
  let ex = Executor.new_executor () in
  Executor.add_task ex main1;
  Executor.add_task ex main2;
  Executor.run_tasks ex
