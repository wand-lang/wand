## 0.90.1 - 2026-09-28

`wand p interface --check` passes the section it writes, and `wand f` keeps a wide arm body under its arm.

### Fixed

- **`wand p interface --check` passes the section that `wand p interface`
  writes.** A record whose fields do not fit on one line is written with a
  line for each field, and the check read each of those lines as an entry
  of its own. It failed on a section written a moment before, and named
  every field as removed. `wand p release` compared entries in the same
  way, so the next release of such a package asked for a major bump that
  nothing needed. Both now read a record over several lines as one entry.

- **`wand f` keeps a wide operator chain in an arm under the arm.** A
  `match` or `handle` arm whose body did not fit on the arrow's line was
  broken at the arm's own indent, so the `|>` sat level with the `|` of
  the arm:

  ```
  | s -> String.to_int s
  |> Result.map (fn n -> n * 1000)
  ```

  The body now goes on its own line, and its operators line up under it:

  ```
  | s ->
    String.to_int s
    |> Result.map (fn n -> n * 1000)
  ```

  A `handle` arm is now measured from after its arrow, as a `match` arm
  is. Before, a `handle` arm whose line was wider than the margin could
  stay on one line.
