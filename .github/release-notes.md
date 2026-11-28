## 0.116.1 - 2026-11-28

### Fixed

- `wand f` no longer adds a space between a qualified constructor and its bracket inside a parameter's brackets. `(shapes.Circle(radius: Int))` gives the bracket to `Circle`. `wand f` wrote it as `(shapes.Circle (radius: Int))`, which is a payload after a space, and the next `wand f` wrote a different file. Now it writes `(shapes.Circle((radius: Int)))` (#230):

  ```
  let area () (shapes.Circle(radius: Int)) = radius
  ```
