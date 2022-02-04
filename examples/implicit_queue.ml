open Fatamorgana
let (let* ) = bind

module Expensive : sig
  include Actor
  val cast : data cast
  val call : (data, unit) call
end = struct
  type data = unit
  let default () = ()
  let cast () =
    print_endline "[expens] in Expensive.cast";
    return ()
  let call () =
    print_endline "[expens] in Expensive.call";
    return ((), ())
end

module Client : sig
  include Actor
  val work : data cast
end = struct
  type data = Expensive.data pid
  let default () = spawn (module Expensive)
  let work pid =
    print_endline "[client] calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "[client] called Expensive.cast";
    print_endline "[client] calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "[client] called Expensive.cast";
    print_endline "[client] calling Expensive.call";
    let* () = call pid Expensive.call in
    print_endline "[client] called Expensive.call";
    print_endline "[client] calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "[client] called Expensive.cast";
    return pid
end

let main =
  let pid = spawn (module Client) in
  print_endline "[  main] calling Client.work";
  let* () = cast pid Client.work in
  print_endline "[  main] called Client.work";
  return ()

let _ =
  let ex = Executor.create () in
  print_endline "[system] scheduling task";
  Executor.add ex main;
  print_endline "[system] starting executor";
  Executor.run_tasks ex
