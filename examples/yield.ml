open Fatamorgana
let (let* ) = bind

module Expensive
: sig
  include Actor
  val cast : data cast
end
= struct
  type data = unit
  let data_format = []
  let default () = ()
  let cast () =
    print_endline "[expens] in Expensive.cast";
    return ()
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
    print_endline "[client] calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "[client] called Expensive.cast";
    print_endline "[client] calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "[client] called Expensive.cast";
    print_endline "[client] calling yield";
    let* () = yield in
    print_endline "[client] called yield";
    print_endline "[client] calling Expensive.cast";
    let* () = cast pid Expensive.cast in
    print_endline "[client] called Expensive.cast";
    return pid
end

let main =
  cast (spawn (module Client)) Client.work

let _ =
  let ex = Executor.create () in
  Executor.add ex main;
  Executor.run_tasks ex
