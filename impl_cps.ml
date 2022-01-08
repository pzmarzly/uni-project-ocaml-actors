open Actor

module CpsMonad : sig
  type 'a t
  type 'data pid

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a

  val spawn : (module Actor with type data = 'data) -> 'data pid t
  val call : 'data pid -> ('data, 'a) call -> 'a t
end = struct
  type 'a t = { run : 'r. ('a -> 'r) -> 'r }
  type 'data pid = 'data ref

  let return x = { run = fun k -> k x }
  let bind a f = { run = fun k -> a.run (fun x -> (f x).run k ) }
  let run a = a.run (fun x -> x)

  let spawn (type data) (module M : Actor with type data = data) =
    { run = fun k -> k (ref M.default) }

  let call pid fn =
    { run = fun k ->
      let data, ret = fn !pid in
      pid := data;
      k ret
    }
end
