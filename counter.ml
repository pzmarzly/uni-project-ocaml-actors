open Actor

module Counter
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

open Impl_id
let main_id =
  let (let* ) = IdMonad.bind in
  let* pid = IdMonad.spawn (module Counter) in
  let* current = IdMonad.call pid Counter.get_current in
  IdMonad.return (Printf.printf "%i\n" current)
open Impl_cps
let main_cps =
  let (let* ) = CpsMonad.bind in
  let* pid = CpsMonad.spawn (module Counter) in
  let* current = CpsMonad.call pid Counter.get_current in
  CpsMonad.return (Printf.printf "%i\n" current)
open Impl_queue
let main_queue =
  let (let* ) = QueueMonad.bind in
  let* pid = QueueMonad.spawn (module Counter) in
  let* current = QueueMonad.call pid Counter.get_current in
  QueueMonad.return (Printf.printf "%i\n" current)

(* pomysły: *)
(* system zatrzymuje się gdy nie ma otwartych fd i wszystkie kolejki są puste - zwraca wtedy unit *)
(* a może explicit funkcja stop? *)
