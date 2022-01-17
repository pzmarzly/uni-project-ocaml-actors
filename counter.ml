open Actor
let (let* ) = M.bind

module Counter
: sig
  include Actor
  val increase : data T.cast
  val get : (data, int) T.call
  val set : int -> (data, int) T.call
end
= struct
  type data = int
  let data_format = [("value", SInt)]
  let default () = 0
  let increase v = M.return (v + 1)
  let get v = M.return (v, v)
  let set new_state state = M.return (new_state, state)
end

module Counters
: sig
  include Actor
  val increase : data T.cast
  val get_current : (data, int) T.call
  val get_total : (data, int) T.call
  val set_current : int -> (data, int) T.call
end
= struct
  type data = Counter.data M.pid * Counter.data M.pid
  let data_format = []

  let default () = M.spawn (module Counter), M.spawn (module Counter)
  let increase (cur, tot) =
    let* () = M.cast cur Counter.increase in
    let* () = M.cast tot Counter.increase in
    M.return (cur, tot)
  let get_current (cur, tot) =
    let* value = M.call cur Counter.get in
    M.return ((cur, tot), value)
  let get_total (cur, tot) =
    let* value = M.call tot Counter.get in
    M.return ((cur, tot), value)
  let set_current new_cur (cur, tot) =
    let* old_cur = M.call cur (Counter.set new_cur) in
    M.return ((cur, tot), old_cur)
end

let main =
  let pid = M.spawn (module Counters) in
  let* () = M.cast pid Counters.increase in
  let* current = M.call pid Counters.get_current in
  M.return (Printf.printf "%i\n" current)

let run () =
  let ex = Executor.new_executor () in
  Executor.add_task ex (M.into_task main);
  Executor.run_tasks ex

(* pomysły: *)
(* system zatrzymuje się gdy nie ma otwartych fd i wszystkie kolejki są puste - zwraca wtedy unit *)
(* a może explicit funkcja stop? *)
