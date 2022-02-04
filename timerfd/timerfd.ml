module Clock = struct
  type t = int
  let realtime = 0
  let monotonic = 1
  let boottime = 7
  let realtime_alarm = 8
  let boottime_alarm = 9
end

external unix_create : int -> int -> int = "unix_create"

let create (id : Clock.t) (ms : int) : Unix.file_descr =
  let fd_as_int = unix_create id ms in
  Obj.magic fd_as_int
