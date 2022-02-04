open Fatamorgana
let (let* ) = bind

module Counter : sig
  include Actor
  val increase : data cast
  val get : (data, int) call
end = struct
  type data = int
  let default () = 0
  let increase v = return (v + 1)
  let get v = return (v, v)
end

module Handler : sig
  include Actor
  val handle : Unix.file_descr -> data cast
end = struct
  type data = int * Counter.data pid
  let default () = Random.int 100000, spawn (module Counter)
  let cmd fd buf (id, cnt) =
    let lower = Bytes.lowercase_ascii buf in
    let command = Bytes.sub_string lower 0 3 in
    Printf.printf "Got command from %d: %s\n%!" id command;
    match command with
    | "inc" -> cast cnt Counter.increase
    | "get" ->
      let* v = call cnt Counter.get in
      Printf.printf "Replying to %d with state %d\n%!" id v;
      let v_str = Printf.sprintf "%d\n" v in
      let v_len = String.length v_str in
      let* () = wait_write fd in
      let num = Unix.write_substring fd v_str 0 v_len in
      assert (num = v_len);
      return ()
    | _ -> return ()
  let rec handle fd state =
    let* () = wait_read fd in
    let buf = Bytes.create 5 in
    let num = Unix.read fd buf 0 4 in
    let* () = if num == 4 then cmd fd buf state else return () in
    let* state = handle fd state in (* TODO: cast_self *)
    return state
end

module Server : sig
  include Actor
  val bind : data pid -> int -> data cast
  val port : (data, int) call
end = struct
  type data = Counter.data pid * int option
  let default () = spawn (module Counter), None

  let rec accept server_fd state =
    let* () = wait_read server_fd in
    let (client_fd, _client_addr) = Unix.accept server_fd in
    let* () = cast (spawn (module Handler)) (Handler.handle client_fd) in
    accept server_fd state (* TODO: cast_self *)

  let bind pid port state =
    match state with
    | cnt, Some x -> failwith "Already bound"
    | cnt, None ->
      let server_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt server_fd Unix.SO_REUSEADDR true;
      Unix.bind server_fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
      Unix.listen server_fd 32;
      let* () = cast pid (accept server_fd) in
      return (cnt, Some port)

  let port (cnt, port) = return ((cnt, port), Option.get port)
end

let main port_in =
  let pid = spawn (module Server) in
  let* () = cast pid (Server.bind pid port_in) in
  let* port_out = call pid Server.port in
  return (Printf.printf "listening on %i\n%!" port_out)

let _ =
  if Array.length Sys.argv < 2 then
    Printf.printf "Please provide port number\n%!"
  else
    let port = Array.get Sys.argv 1 |> int_of_string in
    let ex = Executor.create () in
    Executor.add ex (main port);
    Executor.run_tasks ex
