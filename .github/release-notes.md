## 0.90.2 - 2026-09-28

`wand f` breaks wide condition chains, and puts a wide value below the text before it.

### Fixed

- **`wand f` breaks a wide `&&` or `||` chain.** It stayed on one line
  however wide it was. It now breaks as a pipeline does, one condition on
  each line with the operator first:

  ```
  let inline? j =
    Option.none? (ref_of j)
    && Map.size (obj "properties" j) > 0
    && !(flag? "x-kubernetes-preserve-unknown-fields" j)
  ```

- **`wand f` puts a wide value below the text before it.** After an
  `else`, a field's `name = ` or an arm's `->`, an operator chain or an
  application that did not fit broke at the indent of that line, so its
  second line read as the start of something new:

  ```
    else JSON.decode (Decode.list Diagnostic.decoder) (JSON.parse! r.stdout)
    |> Result.get!
  ```

  It now goes on its own line, two columns in:

  ```
    else
      JSON.decode (Decode.list Diagnostic.decoder) (JSON.parse! r.stdout)
      |> Result.get!
  ```

  An application whose first line opens a bracket or a lambda, such as
  `List.map (fn x ->` or `Decl(`, stays where it starts.

- **`wand f` measures a construction through a module from where it
  starts.** `core.PodTemplateSpec(...)` after `template = ` was measured
  from the indent, and went on one line past the margin.

- **`wand f` opens a one-armed `if`'s block on the `then` line.** When a
  statement in the block did not fit, the `(` went below `then` on a line
  of its own, with the statements at its column.
