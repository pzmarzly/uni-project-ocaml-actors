#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#undef __USE_POSIX199309
#define __USE_POSIX199309

#include <sys/timerfd.h>

CAMLprim value unix_create(value v_clock_id, value v_ms)
{
    CAMLparam2(v_clock_id, v_ms);

    int clock_id = Int_val(v_clock_id);
    if (clock_id < CLOCK_REALTIME || clock_id > CLOCK_BOOTTIME_ALARM)
        caml_invalid_argument("timerfd: clock_id");

    int ms = Int_val(v_ms);
    if (ms < 0)
        caml_invalid_argument("timerfd: ms");

    int fd = timerfd_create(clock_id, 0);
    if (fd < 0)
        caml_failwith("timerfd_create");

    struct itimerspec its = {};
    its.it_value.tv_sec = ms / 1000;
    its.it_value.tv_nsec = ms * 1000 * 1000;

    if (timerfd_settime(fd, 0, &its, NULL) < 0)
        caml_failwith("timerfd_settime");

    CAMLreturn(Val_int(fd));
}
