## 0.91.2 - 2026-09-28

`wand f` opens a `then` block on the `then` line when the `if` has an `else`.

### Fixed

- **`wand f` opens a `then` block on the `then` line when the `if` has an
  `else`.** Before, the `(` went on a line of its own:

  ```
  if List.empty? found then
    (
      IO.println_err
        "no module under k8s/ names the group-versions it came from; run gen first";
      Proc.exit 2
    )
  else found
  ```

  Now it is written the same way as an `if` with no `else`:

  ```
  if List.empty? found then (
    IO.println_err
      "no module under k8s/ names the group-versions it came from; run gen first";
    Proc.exit 2
  )
  else found
  ```
