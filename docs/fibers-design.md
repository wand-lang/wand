# Fibers

wand will run a *service*: one process that handles many requests at the
same time, such as an HTTP API, a worker or a long-running job. This record
is the first of four that do this work:

1. `fibers-design.md` (this record): the scheduler, `Par` on it, and waiting
   versions of the I/O that exists.
2. `shared-design.md`: `Shared`, `Par.all` and `Clock.every`.
3. `server-design.md`: `Net.listen`, `Stream.each_par` and `HTTP.serve`.
4. `child-process-design.md`: two-way child processes and stderr streams.

Each record ships in one release. This record changes no public API.

Not a goal: distributed processes that talk over a network and restart each
other. Erlang/BEAM does that better, and wand does not compete there.

- [What exists](#what-exists)
- [Problems](#problems)
- [Fibers, hidden behind existing modules](#fibers-hidden-behind-existing-modules)
- [Own scheduler; Eio as a reference only](#own-scheduler-eio-as-a-reference-only)
- [Scheduler](#scheduler)
- [Par: an item moves at its first effect](#par-an-item-moves-at-its-first-effect)
- [Questions](#questions)
- [Order](#order)

## What exists

The design builds on these, and keeps their guarantees:

- `Par` (`stdlib/Par.wand`, `par_run` / `par_race` in `lib/evaluator.ml`):
  structured fork-join. `map`, `each`, `race`, `timeout`. No handles, no
  await, workers never outlive the call. The limit is stated at the call
  site. `Par.map` puts a raise in the element's place as an `Error`.
- Cooperative cancellation at evaluator checkpoints, shared with Ctrl-C.
  A script cannot catch its own cancellation.
- `Resource` and `with`: release runs however the scope ends.
- `Stream`: lazy, pull-based sources.
- Effects in types and in the `uses {...}` manifest; handlers built on
  OCaml 5 `Effect.Deep`. Mocks, `--dry-run` and `--trace` are handlers.
- No mutation in the language.

The core guarantee to keep: **moving work into `Par` cannot escape a
handler.**

## Problems

1. Each `Par` worker is a `Domain.spawn` (an OS thread). Domains are few
   (about one per core), so one task per connection does not fit.
2. Effects cannot cross domains. Under a handler, `Par` forwards effects to
   the calling domain one at a time, and `race` / `timeout` are refused.

## Fibers, hidden behind existing modules

- Add lightweight tasks (fibers) that wand's runtime schedules, not the OS.
  All fibers run in one process; thousands per domain.
- Users never see fibers. No `spawn`, no handle, no fiber type. `Par` uses
  them, and later `Stream.each_par` and `HTTP.serve`. Docs, errors and
  `--trace` say "worker" or "branch", never "fiber".
- The language gives the isolation: a raise in one worker is caught for
  that worker. There is no OS isolation; a crash in OCaml or runaway memory
  stops the whole process.
- Shell commands inside a fiber are still child processes. The fiber waits
  on them without blocking other fibers.

## Own scheduler; Eio as a reference only

Write wand's own scheduler on `Unix`. Read Eio (`lib_eio/core`, and
`lib_eio/mock/backend.ml` first) for design, but do not depend on it.

Reasons Eio was not adopted:

- Eio's scheduler sits at the root (`Eio_main.run`), so forked fibers run
  outside the handlers where `Par` was called. That breaks the core
  guarantee.
- Two structured-concurrency models (Eio switches and cancellation vs `Par`,
  checkpoints, `defer_interrupts`, `Resource`) would have to agree on every
  edge case.
- All I/O would move to Eio's APIs, with about ten more dependencies,
  including io_uring C stubs, in the static release build.
- Two backends to test: `eio_posix` on macOS, `eio_linux` on Linux, where
  io_uring is often blocked in containers.

If io_uring speed is needed later, look at Picos before Eio.

## Scheduler

- The scheduler is an effect handler, installed **at the `Par` call site**,
  inside the user's handlers. A fiber's `WandEffect` that the scheduler does
  not handle continues outward to mocks, `--dry-run` and `--trace` on the
  same stack.
- Internal effects: `Suspend` (wait on an fd, a timer or a child process)
  and `Yield`. Structures: run queue, timer heap, `poll` loop. When the run
  queue is empty, `poll` all waiting fds up to the nearest timer.
- A handler outside the scheduler answers a fiber's effect on its own
  stack. A wand handler's case runs there with the state the handler was
  entered with, not the fiber's, so a fiber's cancellation cannot unwind
  the handler. The default handler does not do the work there at all: it
  hands the work back, and the fiber does it at the perform under a
  default handler of its own, so its waits suspend only the fiber.
- **Fiber-local state.** The evaluator keeps state in `Domain.DLS`:
  `ambient_shell_allow`, `ambient_net_allow`, `ambient_file_net`,
  `shell_deadline`, `current_loc`, `cancelled`, `interrupt_taken`,
  `interrupts_deferred`, `regex_literals`. Fibers on one domain share DLS,
  so each value that is per task becomes fiber-local, saved and restored on
  every switch. This is the largest risk in the work. `regex_literals` is a
  cache and stays per domain.
- **Cancellation per fiber.** The checkpoint checks the current fiber's
  flag. A suspended fiber is woken so it sees the cancel and runs its
  `with` releases.
- **Starvation.** A fiber that only computes never waits. A counter in the
  existing checkpoint yields every N steps.
- **Blocking calls need waiting versions.** Sockets and pipes: non-blocking
  fds plus `poll`. Child processes (including `curl` for `HTTP`, and
  `Shell.stream`): wait on the pipes and the exit. Sleep: the timer heap.
  Regular files and DNS cannot use `poll` (see Questions).

## Par: an item moves at its first effect

`Par` keeps its name and API. Its doc becomes: *at the same time; in
parallel when the work is pure.*

The evaluator never sees an inferred effect: the typechecker accepts or
rejects a program, and a compile-cache hit skips inference for an import.
So the runtime decides, from what an item does:

- Every item starts on a **domain pool**, one domain per core at most, so
  work that only computes runs in parallel.
- At its first effect, the item stops on the pool. Its continuation moves
  to the calling domain and runs there as a **fiber**, inside every
  handler the call is inside. The pool domain takes the next item.
- `limit` caps the items in progress, on the pool and as fibers together.
- `Raise` is not an effect here: a raise stays on the pool.
- An item that computes heavily *after* its first effect runs on one core
  with the other fibers. Split it into a pure `Par` call and an effect
  step to get the parallelism back.

No effect is ever performed off the calling domain, so the core guarantee
holds, and a helper that wraps `Par` loses nothing to open effects.

Each position cell is padded to 128 bytes. Every evaluation step writes
one, and two cells on one cache line made two pool domains each run at
half speed.

Later: M:N (fibers across several domains) for CPU-heavy services. Fibers
on other domains are out of handlers' reach, so under a handler they stay
on the calling domain.

## Questions

- Regular files and DNS under fibers: accept a short block, or use a helper
  thread.
- The yield interval N. Measure it on the interpreter loop.

## Order

1. Fiber scheduler in the evaluator: run queue, timer heap, `poll` loop,
   fiber-local state in place of `Domain.DLS`, per-fiber cancellation,
   yield every N steps.
2. `Par` on the new rules: items start on the domain pool and move to a
   fiber at their first effect.
3. Waiting versions of existing I/O: shell, `Shell.stream`, `HTTP` through
   `curl`, sleep.

Step 1 carries most of the risk.
