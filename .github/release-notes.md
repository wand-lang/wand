## 0.88.1 - 2026-09-27

A manifest fix: piping a value into a command needs Shell. And a clearer message for lines read as one expression.

### Fixed

- **A value piped into a command needs `Shell` in the manifest.** `x |>
  $(cmd)` and `x |> $?(cmd)` recorded no effect, so a file whose manifest
  said `uses {IO}` typechecked and ran the command:

  ```
  uses {IO}
  import IO
  let s = "x" |> $(cat)      -- accepted, and a run ran cat
  IO.println s
  ```

  It is refused now: "performs Shell, which the manifest does not allow.
  The manifest should be: "uses {IO, Shell(cat)}"". `wand t --fix` writes
  that line. A file that pipes into a command and declares no `Shell`
  needs its manifest changed.

- **Lines meant as statements, read as one expression, say so.** A `match`
  arm holds one expression, so two statements written one per line in an
  arm were read as one call, and the message named two types. It now quotes
  the lines and writes the fix:

  ```
  | Some n ->
    IO.println "a"
    IO.println "b"
  ```

  Before:

  ```
  expected Unit, got ('a -> Unit ! {IO}) -> 'b
  ```

  Now:

  ```
  lines 6 to 7 are read as one expression, so line 6 is called with line 7 as its argument:
    6 | IO.println "a"
    7 | IO.println "b"
  To run them one after the other, put them in brackets with ';' between them:
    ( IO.println "a"; IO.println "b" )
  ```
