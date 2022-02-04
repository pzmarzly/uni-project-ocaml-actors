module type Actor = sig
    type data
    val default : unit -> data
end

type 'ret monadic
val return : 'a -> 'a monadic
val bind : 'a monadic -> ('a -> 'b monadic) -> 'b monadic
val (let*) : 'a monadic -> ('a -> 'b monadic) -> 'b monadic

type 'data pid
type 'data cast = 'data -> 'data monadic
type ('data, 'ret) call = 'data -> ('data * 'ret) monadic
val spawn : (module Actor with type data = 'a) -> 'a pid
val cast : 'a pid -> 'a cast -> unit monadic
val call : 'a pid -> ('a, 'b) call -> 'b monadic
val wait_read : Unix.file_descr -> unit monadic
val wait_write : Unix.file_descr -> unit monadic
val sleep : int -> unit monadic
val yield : unit monadic

module Executor : sig
    type t
    val create : unit -> t
    val add : t -> unit monadic -> unit
    val run_tasks : t -> unit
end
