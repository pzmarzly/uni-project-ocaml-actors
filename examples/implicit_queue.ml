open Fatamorgana
let (let* ) = bind

module Expensive
: sig
  include Actor
  val cast : data cast
  val call : (data, unit) call
end
= struct
  type data = unit
  let data_format = []
  let default () = ()
  let cast () =
    print_endline "in Expensive.cast";
    return ()
  let call () =
    print_endline "in Expensive.call";
    return ((), ())
end

module Client
: sig
  include Actor
  val work : data cast
end
= struct
  type data = Expensive.data pid
  let data_format = []

  let default () = spawn (module Expensive)
  let work pid =
    print_endline "calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "called Expensive.cast";
    print_endline "calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "called Expensive.cast";
    print_endline "calling Expensive.call";
    let* () = call pid Expensive.call in
    print_endline "called Expensive.call";
    print_endline "calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "called Expensive.cast";
    return pid
end

let main =
  let pid = spawn (module Client) in
  print_endline "calling Client.work";
  let* () = cast pid Client.work in
  print_endline "called Client.work";
  return ()

let _ =
  let ex = Executor.new_executor () in
  print_endline "scheduling task";
  Executor.add ex main;
  print_endline "starting executor";
  Executor.run_tasks ex
