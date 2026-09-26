# Server

The third of four records for services; `fibers-design.md` lists all four.
It needs fibers and `Shared`.

- [Server API](#server-api)
- [Service needs](#service-needs)
- [First real service: the playground eval endpoint](#first-real-service-the-playground-eval-endpoint)
- [Release plan](#release-plan)
- [Questions](#questions)
- [Order](#order)

## Server API

```
uses {Net.Listen(:8080), Shared, IO}

Net.listen :8080
|> Stream.each_par 256 handle
```

- `Net.listen port`: a `Stream` of connections. A new manifest label
  `Net.Listen(port)`, separate from egress `Net(host)`.
- `Stream.each_par limit f s`: like `Par.each` over a stream. `limit` is
  the most connections handled at once. At the limit, accepting stops and
  new clients wait in the OS queue (backpressure). Each item is isolated
  like a `Par.map` element. It runs until the stream ends or is cancelled;
  shutdown is the existing cancellation path.
- `HTTP.serve port limit handler`: `Net.listen` + `Stream.each_par` + HTTP
  parsing.
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
- Deadlines: see Questions. `Par.timeout` works under handlers once `Par`
  runs on fibers.

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
- **Logging.**
    - Writes are line-atomic, so lines from different fibers never mix.
    - A small structured log function (one JSON line per event, with a
      request ID), in `IO` or a new `Log` module.
    - Default: log to stdout and let the platform rotate (journald, Docker,
      Kubernetes).
    - If wand writes log files, rotation is a `Log` setting
      (`rotate = 100MB`, `rotate = 1d`), not a user task: rotation must
      close and reopen the file every write uses, which only the module can
      coordinate safely.
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

## Questions

- `HTTP.Incoming` as a separate type vs reusing `HTTP.Request`.
  Recommended: separate. A client request has a full URL; a server request
  has a path, query and peer, and must not be sendable back out by mistake.
- Where structured logging lives: `IO` or a new `Log` module.
- Per-request deadlines: an `HTTP.serve` argument vs `Par.timeout` in the
  handler.

## Order

1. `Net.listen` and `Stream.each_par`, with the `Net.Listen(port)` label.
2. `HTTP.serve`, with request limits, graceful shutdown, and line-atomic
   logging.
3. Load-test a simple `HTTP.serve`.
