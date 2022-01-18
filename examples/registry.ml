open Fatamorgana
let (let* ) = bind

module Registry (Impl : Actor) : sig
  include (module type of Impl)
  val get : unit -> Impl.data pid
  val set : Impl.data pid -> unit
end = struct
  include Impl
  let instance = ref (spawn (module Impl))
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
  let increase v = return (v + 1)
  let get v = return (v, v)
  let set new_state state = return (new_state, state)
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
  Executor.add ex main1;
  Executor.add ex main2;
  Executor.run_tasks ex
