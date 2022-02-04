open Fatamorgana
let (let* ) = bind

(* Issue: module type of Impl = Actor *)
module Registry (Impl : Actor) : sig
  include (module type of Impl)
  val get : Impl.data pid
end = struct
  include Impl
  let get = spawn (module Impl)
end

module Counter : sig
  include Actor
  val increase : data cast
  val get : (data, int) call
  val set : int -> (data, int) call
end = struct
  type data = int
  let default () = 0
  let increase v = return (v + 1)
  let get v = return (v, v)
  let set new_state state = return (new_state, state)
end

let counter1 = spawn (module Counter)
let counter2 = spawn (module Counter)

let main1 =
  let* () = cast counter1 Counter.increase in
  let* current = call counter1 Counter.get in
  return (Printf.printf "%i\n" current)

let main2 =
  let* () = cast counter1 Counter.increase in
  let* current = call counter1 Counter.get in
  return (Printf.printf "%i\n" current)

let main3 =
  let* () = cast counter2 Counter.increase in
  let* current = call counter2 Counter.get in
  return (Printf.printf "%i\n" current)

let _ =
  let ex = Executor.create () in
  Executor.add ex main1;
  Executor.add ex main2;
  Executor.add ex main3;
  Executor.run_tasks ex
