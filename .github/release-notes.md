## 0.115.4 - 2026-11-20

This release fixes two formatter bugs from 0.115.3. One changed what a file means, and one wrote a file that does not parse.

### Fixed

- `wand f` no longer moves an item into the body of the function above it. In this file, `let name =` is a separate item. After `wand f`, the body of `verify!` was on a line of its own, and the indented `let name =` became part of that body (#226):

  ```
  let verify! some_limit those_files = List.zip those_files (List.map (fn f -> f + some_limit) those_files)
    let name =
  let failures checked = (
    -- c
    checked)
  ```

- In this file, `wand f` wrote `import List;` and kept ` -(1` indented below it. A line indented below a `;`, ending an item, is a parse error (#227):

  ```
  import List
   -(1 -- c
  )
  ```
