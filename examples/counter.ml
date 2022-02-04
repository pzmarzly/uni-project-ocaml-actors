open Fatamorgana

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

module Counters : sig
  include Actor
  val increase : data cast
  val get_current : (data, int) call
  val get_total : (data, int) call
  val set_current : int -> (data, int) call
end = struct
  type data = Counter.data pid * Counter.data pid

  let default () = spawn (module Counter), spawn (module Counter)
  let increase (cur, tot) =
    let* () = cast cur Counter.increase in
    let* () = cast tot Counter.increase in
    return (cur, tot)
  let get_current (cur, tot) =
    let* value = call cur Counter.get in
    return ((cur, tot), value)
  let get_total (cur, tot) =
    let* value = call tot Counter.get in
    return ((cur, tot), value)
  let set_current new_cur (cur, tot) =
    let* old_cur = call cur (Counter.set new_cur) in
    return ((cur, tot), old_cur)
end

let main =
  let pid = spawn (module Counters) in
  let* () = cast pid Counters.increase in
  let* current = call pid Counters.get_current in
  return (Printf.printf "%i\n" current)

let _ =
  let ex = Executor.create () in
  Executor.add ex main;
  Executor.run_tasks ex
