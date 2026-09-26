# Shared state and long-lived branches

The second of four records for services; `fibers-design.md` lists all four.
It needs fibers: `Shared` work runs as fibers on one domain.

- [Shared](#shared)
- [Work next to the server](#work-next-to-the-server)
- [Order](#order)

## Shared

`Shared` is the only way a service keeps state that changes. It is not
named "cell", "ref", "var" or "atom": those carry other languages' rules,
and CLAUDE.md lists `ref` as not-wand.

```
type AppState(users: Map String User, hits: Int)

with Shared.make AppState(users = Map.empty, hits = 0) as state ->
  HTTP.serve :8080 256 (handle state)
```

- Generic: `Shared 'a` holds any type, usually one record with all service
  state.
- `Shared.get s` returns the current value (immutable; no locks to read).
- `Shared.update s f`: `f` is pure, old value to new value. Updates apply
  one at a time, so none is lost, and a change to several fields is atomic.
- **Scoped:** it exists only inside `with Shared.make init as s -> ...`.
  There are no global mutable variables.
- **Passed explicitly:** a function uses shared state only if it receives
  it as an argument.
- **An effect:** reading and updating count under a `Shared` manifest
  label, so the manifest shows the script keeps state, and handlers can
  intercept.
- **`Par` sends it to fibers.** Reading or updating `Shared` is an effect,
  so `Par` runs that work as fibers. This keeps `Shared` on one domain with
  no compare-and-swap.
- **No nesting:** the typechecker rejects a `Shared` whose type contains
  another `Shared`, including inside records and lists. An update must be
  one atomic step; an inner `Shared` would make it two.
- Accepted cost: all updates to one `Shared` are serialized, even when they
  touch different keys. If that becomes a bottleneck, use several
  independent `Shared` values first. Add real concurrent structures only if
  measurement shows the need.

## Work next to the server

Long-lived work that is not a map over a list (cache refresh, cleanup,
polling) runs as another branch of the same structured call. Users never
put anything into the scheduler directly.

```
uses {Net.Listen(:8080), FS.Write, Clock, Shared, IO}

with Shared.make init as state ->
  Par.all [
    fn () -> HTTP.serve :8080 256 (handle state),
    fn () -> Clock.every 1h (fn () -> refresh_cache state),
  ]
```

- **`Par.all`** (fail-fast): runs every branch. If one stops or fails, the
  others are cancelled and the error goes to the caller. Branches with
  effects run as fibers. Shutdown cancels the whole call, and every branch
  runs its releases. No `spawn`; nothing outlives the scope.
- **`Clock.every d f`**: runs `f` every `d`, forever.
    - A failed run is isolated: the error is logged and the next tick runs.
      Otherwise one failed refresh would stop the server through `Par.all`.
      Only the loop itself ending stops the service.
    - Runs do not overlap: if a run takes longer than `d`, the missed tick
      is skipped, not queued.
    - Tested with `Test.with_clock` by advancing time.

A long-running command is a branch the same way:

```
uses {Net.Listen(:8080), Shell(tail), Shared, IO}

with Shared.make init as state ->
  Par.all [
    fn () -> HTTP.serve :8080 256 (handle state),
    fn () ->
      Shell.stream $*(tail -f /var/log/app.log)
      |> Stream.each (fn line -> record state line),
  ]
```

- Waiting for the next line suspends only that branch.
- The child belongs to its branch. Cancel stops the child. If the child
  exits, the stream ends, the branch ends, and `Par.all` stops the service.
  To restart the child, loop around the stream.

## Order

1. `Shared`, with the `Shared` label and the no-nesting rule.
2. `Par.all` and `Clock.every`.
