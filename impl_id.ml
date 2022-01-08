open Actor

module IdMonad : sig
  type 'a t
  type 'data pid

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a

  val spawn : (module Actor with type data = 'data) -> 'data pid t
  val call : 'data pid -> ('data, 'a) call -> 'a t
end = struct
  type 'a t = 'a
  type 'data pid = 'data ref

  let return x = x
  let bind a f = f a
  let run x = x

  let spawn (type data) (module M : Actor with type data = data) =
    ref M.default

  let call pid fn =
    let data, ret = fn !pid in
    pid := data;
    ret
end
