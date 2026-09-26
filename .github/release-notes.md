## 0.82.0 - 2026-09-26

State that changes, work that runs beside a service, and a fix for a race
that could hang.

### Added

- **`Shared`: state that changes.** A `Shared` holds one value for the
  length of a `with`, and changes only through `Shared.update`, given a
  function from the old value to the new one. Updates happen one at a time,
  so none is lost when many run at once.

  ```
  type Counts(hits: Int)

  with Shared.make Counts(hits = 0) as counts -> (
    Par.each 50 (fn _ -> Shared.update counts (fn c -> Counts(c, hits = c.hits + 1)))
      (List.range 1 50);
    (Shared.get counts).hits
  )
  -- 50
  ```

  Reading and updating perform a new effect label, `Shared`, so a file that
  keeps state says so in its manifest, and a handler can answer
  `Shared!get` and `Shared!update`. The function given to `update` performs
  nothing. Using a `Shared` after its `with` ends raises.

  A `Shared` cannot hold another `Shared`, in a list, a map, a record field
  or anywhere else:

  ```
  with Shared.make 0 as a -> with Shared.make [a] as b -> ...
  -- type error: a Shared cannot hold another Shared, and this one holds
  --             List (Shared Int)
  ```

- **`Par.all!`: run branches side by side until one ends.** For work that
  runs for as long as a script does: when one branch ends, the others stop
  and give back what they hold. A branch that raised makes `Par.all!` raise
  the same failure, so a script whose work dies exits with an error instead
  of going quiet.

  ```
  let watch! log = with Shared.make 0 as lines ->
    Par.all! [
      fn () -> Shell.stream $*(tail -f %{log})
        |> Stream.each (fn _ -> Shared.update lines (fn n -> n + 1)),
      fn () -> Clock.every 5min (fn () -> IO.println "%{Shared.get lines} lines so far")
    ]
  ```

  `Par.race` still returns the first failure as a `Result`.

- **`Clock.every`: run a function every period, forever.** The first run is
  at once. Runs never overlap, and a run longer than the period skips the
  ticks it missed instead of making the next runs catch up. A run that
  raises ends `Clock.every` with that failure. To go on after a failed run,
  catch it with `try` in the function.

### Fixed

- **A race could hang on a branch reading a silent command.** A branch
  waiting on `Shell.stream` for a line that never came did not stop when
  another branch won, so `Par.race` and `Par.timeout` never returned. It
  now stops, and its command is killed.

  ```
  Par.timeout 200ms (fn () ->
    Shell.stream $*(sh -c 'sleep 30') |> Stream.each (fn _ -> ()))

  -- before    never returns
  -- now       Error("timed out after 200ms")
  ```
