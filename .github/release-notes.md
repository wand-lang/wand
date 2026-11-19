## 0.115.3 - 2026-11-19

This release fixes a formatter bug that changed what a file means.

### Fixed

- `wand f` no longer moves an indented `let ... in` to column 1 when a comment inside it makes `wand f` keep it as written. At column 1, the `let ... in` took the next line as part of its body. In this file, `let b` is a separate item, but after `wand f` it was part of the body of `v` (#225):

  ```
   let v () = (e --
  ) in ()
  let b = import /e
  ```
