# Server

wand will run a *service*: one process that handles many requests at the
same time, such as an HTTP API, a worker or a long-running job. The work is
in four records, each released once:

1. Fibers: the scheduler, `Par` on it, and waiting versions of the I/O
   that exists. Released in 0.81.0.
2. Shared state: `Shared`, `Par.all!` and `Clock.every`. Released in
   0.82.0.
3. `server-design.md` (this record): `Net.listen`, `Stream.each_par` and
   `HTTP.serve`.
4. `child-process-design.md`: two-way child processes and stderr streams.

Not a goal: distributed processes that talk over a network and restart each
other. Erlang/BEAM does that better, and wand does not compete there.

- [Server API](#server-api)
- [Service needs](#service-needs)
- [First real service: the playground eval endpoint](#first-real-service-the-playground-eval-endpoint)
- [Release plan](#release-plan)
- [Order](#order)

## Server API

```
uses {Net.Listen(:8080), Shared, IO}

Net.listen :8080
|> Stream.each_par 256 handle
```

- `Net.listen port`: a `Stream` of connections. A new manifest label
  `Net.Listen(port)`, separate from egress `Net(host)`.
- `Stream.each_par limit f s`: runs `f` on each element side by side, on
  the calling domain. `limit` is the most handled at once. At the limit,
  reading stops, so accepting stops and new clients wait in the OS queue
  (backpressure). An element that raises stops the others and the read,
  and is raised: a caller that wants to go on past one catches it in `f`,
  as `Clock.every` asks. `HTTP.serve` does that catching, turning a raise
  into a 500. An element that is a connection is closed when `f` ends.
- Connections: `Net.read_line`, `Net.read`, `Net.write`, `Net.write!` and
  `Net.peer`. Reads and writes are operations under `Net.Listen`.
- `HTTP.serve! server handler`: `Net.listen` + `Stream.each_par` + HTTP
  parsing, as an `HTTP.Server` record says.
- TLS is out of scope for v1; terminate it at a proxy.

A handler is a plain function from request to response:

```
let handle state (req: HTTP.Incoming) =
  match (req.method, HTTP.segments req) with
  | (GET, ["users", id]) -> (
      match Map.get id (Shared.get state).users with
      | Some u -> HTTP.reply 200 (JSON.stringify (JSON.of! u))
      | None -> HTTP.reply 404 "")
  | (POST, ["users"]) -> (
      match JSON.decode User.decoder (JSON.parse! req.body) with
      | Ok u -> (
          Shared.update state (fn s -> AppState(s, users = Map.set u.id u s.users));
          HTTP.reply 201 "")
      | Error why -> HTTP.reply 400 why)
  | _ -> HTTP.reply 404 ""
```

- Routing is `match` on method and path segments; no router DSL.
- Expected failures are values turned into 4xx. A raise becomes a 500 for
  that request only.
- Deadlines: a per-request deadline is an argument of `HTTP.serve`, like
  the request limits. A request past it is cancelled and answered 503, and
  its releases run.
- `HTTP.Incoming` is a type of its own, not `HTTP.Request`: a client
  request has a full URL, and a server request has a path, a query and a
  peer. It cannot be sent back out by mistake.

Tests call the handler directly, with no socket:

```
test "unknown user is 404" (fn t ->
  with Shared.make AppState(users = Map.empty, hits = 0) as state ->
    t.eq 404 (handle state (HTTP.incoming GET "/users/9")).status)
```

Outbound calls from a handler are mocked with `Test.with_http` as today.

New names: `Net.listen`, `Net.Listen`, `Stream.each_par`, `HTTP.serve`,
`HTTP.Incoming`, `HTTP.incoming`, `HTTP.segments`, `HTTP.reply`.

## Service needs

Needed for a first real service:

- **Outbound HTTP stays on `curl` for now.** `curl` already does TLS, and
  with fibers a `curl` call waits on its pipes without blocking other
  requests. The cost is speed: a process per call, no connection reuse.
  TLS is deferred until after the load test.
- **Graceful shutdown.** On SIGTERM: stop accepting, let in-flight requests
  finish up to a deadline, then cancel the rest and run their releases.
  Signals, cancellation and `with` exist; `HTTP.serve` connects them.
- **Request limits as defaults** on `HTTP.serve`: max body size, max header
  size, timeout for reading headers (against slow clients). Use `Size` and
  `Duration` literals: `max_body = 1MB`, `header_timeout = 10s`.
- **Line-atomic writes.** Lines from different requests never mix. Each
  `IO.println` is one operation on one domain, and a fiber cannot switch
  inside one, so this is expected to hold already; the load test confirms
  it. A structured log function and log files are the `Log` module, after
  1.0.
- **`--dry-run` and `--trace` for servers.** Dry run: typecheck, print
  "would listen on :8080", exit. Trace on a long-running process needs a
  way to limit its output.

Needed soon after:

- **Request parsing helpers** on `HTTP.Incoming`: query strings, form
  bodies, headers.

Check before building more:

- **Interpreter speed and long-running behavior.** The tree-walking
  interpreter may limit throughput, and caches built for short scripts
  (`compile_cache`, `regex_literals`) may grow forever in a process that
  never exits. Load-test a simple `HTTP.serve` early.

Not needed: restarts and supervision (systemd or Kubernetes do this for a
single process), WebSockets and streaming responses (later), database
drivers and connection pools (not in this work).

## First real service: the playground eval endpoint

The website's playground runs wand code sent from the browser. Its eval
endpoint is written in wand, on this design, as the test of the design. If
something is awkward to write here, the design changes before it is frozen.

It uses most of the new work: `HTTP.serve`, request limits, per-request
deadlines, child processes, and `Par` on fibers.

It is built **after package management**, in a **separate repo**, so it
also tests depending on wand through the package manager. The simple
`HTTP.serve` load test runs as part of this record, without waiting for
the endpoint.

It runs code from strangers, so these rules hold:

- **Effects.** Refuse any submitted script whose manifest declares more
  than a small allowed set (expected: `IO` only). The manifest check
  (`wand t`) does the hard part; a script that declares too little already
  fails to typecheck.
- **CPU.** Every evaluation has a deadline. The yield checkpoint lets the
  deadline stop pure computation, not only waiting work.
- **Memory.** Fibers give no memory isolation, so evaluations do not run
  inside the service process. Each one runs in a child `wand` process with
  OS limits (`ulimit`, `prlimit`, or a container), and the service manages
  the children. `Shell.timeout` bounds the child as well.
- **Input size.** `HTTP.serve` request limits cap the submitted source
  (`max_body`).
- **Files.** Each evaluation gets its own temporary directory through
  `with FS.temp_dir`, released however the request ends.

Rough shape (names from this design; not final syntax):

```
uses {Net.Listen(:8080), Shell(wand), FS.Write, IO}

let run_source src =
  with FS.temp_dir "eval-" as dir -> (
    FS.write_file! (dir / "main.wand") src;
    match $?(wand t %{dir / "main.wand"}) with
    | r when Shell.failed? r -> HTTP.reply 400 r.stderr
    | _ -> (
        match Shell.timeout 5s (fn () -> $?(wand %{dir / "main.wand"})) with
        | Ok r -> HTTP.reply 200 r.stdout
        | Error why -> HTTP.reply 408 why))

let handle (req: HTTP.Incoming) =
  match (req.method, HTTP.segments req) with
  | (POST, ["eval"]) -> run_source req.body
  | _ -> HTTP.reply 404 ""

HTTP.serve :8080 64 handle
```

Still to add in the real version: the manifest allowlist check (more than
"does it typecheck"), OS memory limits on the child, and the output size
limit.

Record while building it: every place the design was awkward, every
missing stdlib function, and the load-test numbers. Those decide the TLS
question and whether the services API changes before 1.0.

## Release plan

Services first (these four records), then package management, then the
playground eval service (separate repo), then 1.0, then `Log`. Versions
stay in 0.x minors until 1.0.

## Order

1. `Net.listen` and `Stream.each_par`, with the `Net.Listen(port)` label.
   Done (10ab71b).
2. `HTTP.serve`, with request limits, a per-request deadline and graceful
   shutdown. Done: `HTTP.serve! HTTP.Server(port = :8080) route`. The
   server is a record with defaults, as a client's `HTTP.Request` is, so a
   new setting is a new field. The name carries `!` because listening
   raises when the port is taken. Each connection answers one request and
   closes; keeping a connection open for more is not in this record.
3. Load-test a simple `HTTP.serve`. Done 2026-09-26, results below.

## Load test

Measured with `ab` against `HTTP.serve!` on this machine (16 cores, OCaml
5.5.1), one connection per request:

| Handler | Clients | Requests a second | p99 |
|---|---|---|---|
| `HTTP.reply 200 "ok"` | 16 | 6,677 | 5 ms |
| `HTTP.reply 200 "ok"` | 64 | 8,883 | 19 ms |
| reads a 4 KB file | 16 | 5,525 | 7 ms |
| computes `fib 15` | 16 | 4,392 | 11 ms |
| one outbound `HTTP.get` | 16 | 360 | 56 ms |

For scale, Python's `ThreadingHTTPServer` answered about 3,500 a second
with p99 32 ms and worst cases over a second; a bare `Net.listen` and
`Stream.each_par` answers about 22,000.

- **Correct under load.** No request failed at 1 to 256 clients; 5,000
  concurrent `Shared` updates counted 5,000; 5,000 concurrent log lines
  were all intact, so writes are line-atomic.
- **Memory is flat:** 23 MB after 80,000 requests. It first grew 9 KB a
  request, from spawning domains per `Par` call and, before OCaml 5.5,
  from fiber stacks freed on another domain. `Par` now keeps a pool, and
  wand builds on 5.5.1.
- **File reads under fibers:** a handler that reads a file costs about a
  quarter of the throughput and nothing in latency. The short block is
  confirmed.
- **Outbound HTTP:** a `curl` per call costs about 17 ms and caps a
  handler that calls out at about 360 a second. That is enough for a
  service that calls another now and then, so outbound HTTP stays on
  `curl`; a native client for plain HTTP waits for a service that calls
  out on every request.
- **Where the time goes:** each request makes two `Par.timeout` calls, for
  the head and for the deadline. Handing each to the domain pool capped a
  server near 4,400 a second; with no races at all a copy of the serving
  code answers about 14,000. `Par` now runs a function on the calling
  domain when it was quick the last time, so only a handler that computes
  for a while goes to the pool. The best of three rounds is above, measured
  with the machine at a load average of about 10.
