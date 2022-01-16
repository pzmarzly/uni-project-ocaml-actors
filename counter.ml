open Actor

module Counter
: sig
  include Actor
  val increase : data T.cast
  val set : int -> (data, int) T.call
end
= struct
  type data = int
  let data_format = [("value", SInt)]
  let default = 0
  let increase v = M.return (v + 1)
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
  type data = int * int
  let data_format = [("current", SInt); ("total", SInt)]

  let default = (0, 0)
  let increase (state1, state2) = M.return (state1 + 1, state2 + 1)
  let get_current (state1, state2) = M.return ((state1, state2), state1)
  let get_total (state1, state2) = M.return ((state1, state2), state2)
  let set_current new_state1 (state1, state2) = M.return ((new_state1, state2), state1)
end

let main =
  let (let* ) = M.bind in
  let* pid = M.spawn (module Counters) in
  let* current = M.call pid Counters.get_current in
  M.return (Printf.printf "%i\n" current)

let run () =
  let ex = Executor.new_executor () in
  Executor.add_task ex (M.into_task main);
  Executor.run_tasks ex

(* pomysły: *)
(* system zatrzymuje się gdy nie ma otwartych fd i wszystkie kolejki są puste - zwraca wtedy unit *)
(* a może explicit funkcja stop? *)
