# Fatamorgana - a toy actor framework for OCaml

This is a library that I wrote as a final project for a [Functional Programming course](https://zapisy.ii.uni.wroc.pl/courses/programowanie-funkcyjne-202122-zimowy). It lets programmer to write programs using actors (state + inbox) as an abstraction. From the [`server.ml` example](examples/server.ml):

```ocaml
module Counter : sig
  include Actor
  val increase : data cast
  val get : (data, int) call
end = struct
  type data = int
  let default () = 0
  let increase v = return (v + 1) (* new state *)
  let get v = return (v, v) (* new state, return value *)
end

module Handler : sig
  include Actor
  val handle : Unix.file_descr -> data cast
end = struct
  type data = int * Counter.data pid
  let default () = Random.int 100000, spawn (module Counter)
  let cmd fd buf (id, cnt) =
    let command = ... buf ... in
    match command with
    | "inc" -> cast cnt Counter.increase
    | "get" ->
      let* v = call cnt Counter.get in
      Printf.printf "Replying to %d with state %d\n%!" id v;
      let* () = wait_write fd in
      Unix.write ...;
      return ()
    | _ -> return ()
end

let main (port : int) : unit monadic = ...

let _ =
  let ex = Executor.create () in
  Executor.add ex (main port);
  Executor.run_tasks ex
```

Features:

- cast vs call distinction (one-way vs bidirectional messaging)
- strongly typed, not much boilerplate
- transparent cooperative concurrency with optional manual yielding
- message order guarantees
- asynchronous I/O (`wait_read` and `wait_write` via `Unix.select`)
- timers (via [`timerfd` bindings](timerfd/timerfd.c))
- temporal locality (actor inbox is fully processed before processing next actor)
- `Executor.run_tasks` quits once everything is done
- nothing happens unless a task is scheduler within `Executor`

Other than that, it's very much NOT a state-of-the-art library. If I had to do it again from scratch, I would probably:

- adopt [`libprocess`](https://www.youtube.com/watch?v=P6Y-Z1uPp4c) explicit scheduling scheme with `On(self(), fun () -> ...)`
- at the very least, pass `pid` to actor functions... right now [`cast_self` is unnecessarily hard](examples/cast_self.ml)
