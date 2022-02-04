open Fatamorgana
let main =
  let start = Unix.gettimeofday() in
  assert (Unix.gettimeofday() -. start < 0.1);
  let* () = sleep 100 in
  assert (Unix.gettimeofday() -. start > 0.1);
  print_endline "ok";
  return ()

let _ =
  let ex = Executor.create () in
  Executor.add ex main;
  Executor.run_tasks ex
