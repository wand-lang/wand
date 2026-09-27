## 0.83.0 - 2026-09-26

A server: `HTTP.serve!`, and `Net.listen` with `Par.each_stream` beneath it.
`Par` is faster and no longer grows in memory.


### Added

- **`HTTP.serve!`: a HTTP server.** A handler is a plain function from a request
  to a response, and routing is a `match` on the method and the path.

  ```
  uses {Clock, Net.Listen(:8080), Shared}

  type State(users: Map String)

  let route (state: Shared State) (req: HTTP.Incoming) =
    match (req.method, HTTP.segments req) with
    | (HTTP.GET, ["users", id]) -> (
      match Map.get id (Shared.get state).users with
      | Some name -> HTTP.reply 200 name
      | None -> HTTP.reply 404 ""
    )
    | _ -> HTTP.reply 404 ""

  with Shared.make State(users = {}) as state ->
    HTTP.serve! HTTP.Server(port = :8080) (route state)
  ```

  `HTTP.Server` holds the settings, and only `port` is required:
  `limit = 256` requests at once, `max_body = 1MB`, `max_head = 16KB`,
  `head_timeout = 10s`, `deadline = 30s` and `grace = 10s`. A handler that
  raises answers 500 for that request only; one past the deadline is
  stopped and answers 503; a head that does not arrive in time is 408; a
  body or head over its limit is 413 or 431.

  On SIGTERM or Ctrl-C the server stops accepting, gives the requests in
  progress `grace` to finish, then stops the rest.

  A handler is tested with a request made by `HTTP.incoming`, and no
  socket:

  ```
  t.eq 404 (route state (HTTP.incoming HTTP.GET "/users/9")).status
  ```

  On this machine a server that answers at once handles about 6,500
  requests a second at 16 clients, and its memory stays flat.

- **`Net.listen` and `Par.each_stream`: serving without HTTP.** `Net.listen`
  is a stream of the connections a port accepts, and `Par.each_stream` runs
  a function on each element side by side, at most `limit` at a time, and
  closes a connection when its function ends.

  ```
  let echo! conn =
    match Net.read_line conn with
    | Some line -> Net.write! "%{line}\n" conn
    | None -> ()

  Net.listen :9000 |> Par.each_stream 64 echo!
  ```

  `Net.read_line`, `Net.read`, `Net.write`, `Net.write!` and `Net.peer`
  work on a connection.

- **`Net.Listen`: a twelfth effect label.** Listening on a port is declared
  apart from sending with `Net`, and narrows by port:

  ```
  uses {Net.Listen(:8080)}

  Net.listen :9000 |> Par.each_stream 1 serve

  -- raises: listening on :9000, which Net.Listen(:8080) does not allow
  ```

### Changed

- **`Par` is faster.** It keeps a pool of domains for as long as there is
  work, instead of starting new ones on every call, and a function that
  finished quickly the last time runs without the pool at all.

  ```
  Par.timeout 1s (fn () -> 1)

  -- before    about 180 microseconds
  -- now       about 30, or less for a function Par has seen be quick
  ```

- **Stopping a branch stops the `Par` inside it.** The loser of a race, or
  a branch `Par.all!` stops, used to run any `Par` it had started to the
  end.

  ```
  Par.race [fn () -> (Par.each 4 (fn _ -> Clock.sleep 3s) [1, 2, 3, 4]; "slept"),
            fn () -> "fast"]

  -- before    Ok("fast") after 6 seconds
  -- now       Ok("fast") at once
  ```

- **wand builds on OCaml 5.5.1, with every dependency locked.** Building
  from source needs OCaml 5.5.1; `wand.opam.locked` names every package, and
  `opam install . --deps-only --locked` installs them.

### Fixed

- **A script that called `Par` many times grew in memory.** Each call left
  about 4 KB behind.

  ```
  List.each (fn _ -> Par.map 1 (fn x -> x) [1]) (List.range 1 40000)

  -- before    161 MB
  -- now       9 MB
  ```

- **A `Par` inside a `Par.all!` branch could hold up the other branches.**

  ```
  Par.all! [fn () -> (Par.each 2 (fn _ -> Clock.sleep 1s) [1, 2]; "a"),
            fn () -> (Clock.sleep 100ms; "b")]

  -- before    "b" after 2 seconds
  -- now       "b" after 100 milliseconds
  ```
