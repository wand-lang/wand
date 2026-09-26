## 0.81.0 - 2026-09-26

`Par` runs pure work in parallel and waiting work concurrently, and it works
the same way under a handler.


### Changed

- **`Par` runs work in parallel and concurrently.** Work that only
  computes runs in parallel, on one domain per core. Work that runs
  commands, sleeps or reads streams runs concurrently on the calling
  domain, and a wait holds no thread. So a limit is no longer bounded by
  the number of cores.

  ```
  Par.map 300 (fn host -> $(ssh %{host} uptime)) hosts

  -- before    Error: failed to allocate domain
  -- now       300 commands run concurrently
  ```

  Eight items of pure work finish in half the time they did.

- **A handler around `Par` no longer makes its work take turns.** Under a
  mock, `--dry-run` or `--trace`, every item's effects still reach the
  handler, and now their waits overlap.

  ```
  Test.with_shell mocks (fn () ->
    Par.map 10 (fn h -> (Clock.sleep 300ms; probe h)) hosts)

  -- before    3 seconds: one item at a time
  -- now       300 milliseconds
  ```

- **`Par.race` and `Par.timeout` run inside a handler.** A race answers with its
  real winner, and a deadline fires. `Par.timeout` is still refused under
  `Test.with_clock`, where a sleep takes no time and the deadline would pass
  at once.

  ```
  Test.with_shell mocks (fn () -> Par.race [fn () -> probe a, fn () -> probe b])

  -- before    error: a race inside a handler runs its first thunk only
  -- now       the first probe to finish
  ```
