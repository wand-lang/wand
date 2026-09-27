# Child processes

wand runs a *service*: one process that handles many requests at the same
time. The work was in four records, each released once:

1. Fibers: the scheduler, `Par` on it, and waiting versions of the I/O
   that exists. Released in 0.81.0.
2. Shared state: `Shared`, `Par.all!` and `Clock.every`. Released in
   0.82.0.
3. The server: `Net.listen`, `Par.each_stream` and `HTTP.serve!`, and a
   load test. Released in 0.83.0.
4. `child-process-design.md` (this record): two-way child processes and
   stderr streams.

The load test settled the outbound HTTP question below: a `curl` per call
costs about 17 ms, and a handler that calls out on every request tops out
near 360 requests a second.

Reading a long-running command already exists: `Shell.stream` (stdout, one
line at a time) and `IO.stdin_lines` (wand's own stdin). Two things are
missing:

- **Two-way child processes** (a language server, a REPL, a worker that
  takes jobs on stdin): a `Resource` that gives a line writer and a line
  stream, with the child stopped on release.
- **stderr as a stream.** Only stdout is streamed today.

## Outbound HTTP

Outbound HTTP stays on `curl`: the load test found about 360 calls a
second, enough for a service that calls another now and then. A service
that calls out on every request is what would ask for more, and the first
step then is a native client for plain HTTP only (internal services), with
`curl` kept for HTTPS. The
TLS choice, pure OCaml (`ocaml-tls`, easier static builds) vs OpenSSL
bindings (faster, harder to link statically), is made only when HTTPS calls
themselves are the bottleneck.

## Questions

- The shape of the two-way child-process `Resource`.
- TLS for outbound HTTP (`ocaml-tls` vs OpenSSL bindings), when a native
  client is built.

## Order

1. Two-way child processes.
2. stderr streams.
3. Native outbound HTTP, only when a service needs more than `curl` gives:
   plain HTTP first, TLS later. The load test did not ask for it.
