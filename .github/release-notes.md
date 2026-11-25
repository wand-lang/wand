## 0.116.0 - 2026-11-25

This release fixes a bug where `wand f` changed what a file means. A pattern that was valid before can now be an error.

### Changed

- In a pattern, a constructor with named fields refuses a payload after a space, with or without a bracket. `Circle (r)` was an error and `Circle r` was not, and `wand f` writes the first as the second. Write the fields against the name: `Circle(r)` (#229):

  ```
  type S = Circle(r: Int) | D
  let a s = match s with | Circle (r) -> r | D -> 0
  ```
