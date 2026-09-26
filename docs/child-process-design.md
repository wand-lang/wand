# Child processes

The fourth of four records for services; `fibers-design.md` lists all four.
Part of it waits on the load test in `server-design.md`.

Reading a long-running command already exists: `Shell.stream` (stdout, one
line at a time) and `IO.stdin_lines` (wand's own stdin). Two things are
missing:

- **Two-way child processes** (a language server, a REPL, a worker that
  takes jobs on stdin): a `Resource` that gives a line writer and a line
  stream, with the child stopped on release.
- **stderr as a stream.** Only stdout is streamed today.

## Outbound HTTP

Outbound HTTP stays on `curl` unless the load test shows that one `curl`
per call is too slow. If it is too slow, the first step is a native client
for plain HTTP only (internal services), and `curl` stays for HTTPS. The
TLS choice, pure OCaml (`ocaml-tls`, easier static builds) vs OpenSSL
bindings (faster, harder to link statically), is made only when HTTPS calls
themselves are the bottleneck.

## Questions

- The shape of the two-way child-process `Resource`.
- TLS for outbound HTTP (`ocaml-tls` vs OpenSSL bindings). Deferred until
  after the `HTTP.serve` load test.

## Order

1. Two-way child processes.
2. stderr streams.
3. Native outbound HTTP, only if the load test asks for it: plain HTTP
   first, TLS later.
