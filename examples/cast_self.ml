open Fatamorgana

module Counter : sig
  include Actor
  val increase : data cast
  val add_to_five_eager : data cast
  val add_to_ten_lazy : data pid -> data cast
  val get : (data, int) call
end = struct
  type data = int
  let default () = 0
  let increase v = return (v + 1)
  let rec add_to_five_eager v =
    if v < 5
      then add_to_five_eager (v + 1)
      else return v
  let rec add_to_ten_lazy pid v =
    let* () = if v < 9
      then cast pid (add_to_ten_lazy pid)
      else return () in
    return (v + 1)
  let get v = return (v, v)
end

let x = ref None
module CounterSingleton : sig
  include Actor with type data = int
  val instance : unit -> data pid
  val increase : data cast
  val add_to_fifteen_lazy : data cast
  val get : (data, int) call
end = struct
  type data = int
  let default () = 0
  let instance () = Option.get !x
  let increase v = return (v + 1)
  let rec add_to_fifteen_lazy v =
    let* () = if v < 14
      then cast (instance ()) add_to_fifteen_lazy
      else return () in
    return (v + 1)
  let get v = return (v, v)
end
let _ = x := Some (spawn (module CounterSingleton))

let main =
  let pid = spawn (module Counter) in

  let* () = cast pid Counter.add_to_five_eager in
  let* value = call pid Counter.get in
  Printf.printf "%i\n" value;
  Printf.printf "\n";

  let* () = cast pid (Counter.add_to_ten_lazy pid) in
  let* value = call pid Counter.get in
  Printf.printf "%i\n" value;
  let* () = yield in
  let* () = yield in
  let* () = yield in
  let* () = yield in
  let* value = call pid Counter.get in
  Printf.printf "%i\n" value;
  Printf.printf "\n";

  let* () = cast (CounterSingleton.instance ()) CounterSingleton.add_to_fifteen_lazy in
  let* value = call (CounterSingleton.instance ()) CounterSingleton.get in
  Printf.printf "%i\n" value;
  let wait = ref (return ()) in
  for _ = 1 to 20 do
    wait := bind !wait (fun () -> yield)
  done;
  let* () = !wait in
  let* value = call (CounterSingleton.instance ()) CounterSingleton.get in
  Printf.printf "%i\n" value;

  return ()

let _ =
  let ex = Executor.create () in
  Executor.add ex main;
  Executor.run_tasks ex
