module Clock : sig
  type t
  val realtime : t
  val monotonic : t
  val boottime : t
  val realtime_alarm : t
  val boottime_alarm : t
end

val create : Clock.t -> int -> Unix.file_descr
