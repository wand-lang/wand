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
  takes jobs on stdin).
- **stderr as a stream.** Only stdout is streamed today.

## Two-way child processes

`Shell.spawn cmd` is a `Resource` whose value is the running process. It
is read and written the way a `Net` connection is:

```
uses {Shell(python3)}

with Shell.spawn $*(python3 -i -q) as py -> (
  Shell.write! "print(6 * 7)\n" py;
  Shell.read_line py                -- Some "42"
)
```

- `Shell.read_line p` answers the next stdout line, `None` at its end.
  `Shell.read n p` answers up to `n` bytes, for a protocol framed by
  length rather than by line, as a language server's is.
- `Shell.write text p` answers `Error` when the child no longer reads;
  `Shell.write!` raises.
- `Shell.read_err_line p` and `Shell.read_err n p` read stderr, apart
  from stdout, so a script can tell a result from a warning.
- `Shell.close p` closes stdin, waits for the child to end, and answers a
  `ShellResult` with its exit code and the stderr it has not read.
- The release stops a child that was not closed: stdin closed, then
  SIGTERM, then SIGKILL after the grace `Shell.timeout` gives.
- A read or a write waits in its own fiber. A timeout is `Par.timeout`
  around a read; a worker written to and read from at once is two
  branches of `Par.all!`.
- The effects are `Shell`, narrowed by the command word like any other
  command, and the operations are ones a handler can answer.

## stderr streams

`Shell.stream_err cmd` reads a command's stderr line by line, as
`Shell.stream` reads its stdout; its stdout goes to wand's own.

## Outbound HTTP

Outbound HTTP stays on `curl`: the load test found about 360 calls a
second, enough for a service that calls another now and then. A service
that calls out on every request is what would ask for more, and the first
step then is a native client for plain HTTP only (internal services), with
`curl` kept for HTTPS. The TLS choice, pure OCaml (`ocaml-tls`, easier static builds) vs OpenSSL
bindings (faster, harder to link statically), is made only when HTTPS calls
themselves are the bottleneck.

## Questions

- TLS for outbound HTTP (`ocaml-tls` vs OpenSSL bindings), when a native
  client is built.

## Order

1. Two-way child processes. Done.
2. stderr streams. Done.
3. Native outbound HTTP, only when a service needs more than `curl` gives:
   plain HTTP first, TLS later. The load test did not ask for it.
