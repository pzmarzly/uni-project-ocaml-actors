type serializer =
| SBool
| SInt
| SList of serializer

type 'data cast = 'data -> 'data
type ('data, 'ret) call = 'data -> 'data * 'ret

module type Actor = sig
  type data
  val data_format : (string * serializer) list
  val default : data
end
