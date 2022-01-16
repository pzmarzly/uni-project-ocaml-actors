open Actor

module Counter
: sig
  include Actor
  val increase : data cast
  val set : int -> (data, int) call
end
= struct
  type data = int
  let data_format = [("value", SInt)]
  let default = 0
  let increase v = v + 1
  let set new_state state = new_state, state
end

module Counters
: sig
  include Actor
  val increase : data cast
  val get_current : (data, int) call
  val get_total : (data, int) call
  val set_current : int -> (data, int) call
end
= struct
  type data = int * int
  let data_format = [("current", SInt); ("total", SInt)]

  let default = (0, 0)
  let increase (state1, state2) = (state1 + 1, state2 + 1)
  let get_current (state1, state2) = (state1, state2), state1
  let get_total (state1, state2) = (state1, state2), state2
  let set_current new_state1 (state1, state2) = (new_state1, state2), state1
end

let main =
  let (let* ) = QueueMonad.bind in
  let* pid = QueueMonad.spawn (module Counters) in
  let* current = QueueMonad.call pid Counters.get_current in
  QueueMonad.return (Printf.printf "%i\n" current)

let run () =
  let ex = Executor.new_executor () in
  Executor.add_task ex (QueueMonad.into_task main);
  Executor.run_tasks ex

(* pomysły: *)
(* system zatrzymuje się gdy nie ma otwartych fd i wszystkie kolejki są puste - zwraca wtedy unit *)
(* a może explicit funkcja stop? *)
