open Fatamorgana
let (let* ) = bind

module Counter
: sig
  include Actor
  val increase : data cast
  val get : (data, int) call
end
= struct
  type data = int
  let data_format = [("value", SInt)]
  let default () = 0
  let increase v = return (v + 1)
  let get v = return (v, v)
end

module Server
: sig
  include Actor
  val bind : int -> (data, unit) call
  val port : (data, int option) call
end
= struct
  type data = Counter.data pid * int option
  let data_format = []
  let default () = spawn (module Counter), None
  let bind port state =
    match state with
    (* TODO: bind *)
    | pid, None -> return ((pid, Some port), ())
    | pid, Some x -> failwith "already bound"
  let port (pid, port) = return ((pid, port), port)
end

let main port_in =
  let pid = spawn (module Server) in
  let* () = call pid (Server.bind port_in) in
  let* port_out = call pid Server.port in
  let port_out = Option.get port_out in
  return (Printf.printf "listening on %i\n" port_out)

let _ =
  if Array.length Sys.argv < 2 then
    print_endline "please provide port number"
  else
    let port = Array.get Sys.argv 1 |> int_of_string in
    let ex = Executor.new_executor () in
    Executor.add_task ex (main port);
    Executor.run_tasks ex
