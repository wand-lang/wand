## 0.109.0 - 2026-10-20

This release changes `in` so that it never narrows a name, as in OCaml, and fixes a formatter bug. One change can make an existing file behave differently: read Upgrading.

### Upgrading

- `in` now gives the name to the rest of the body, past a `;`, as in OCaml. Before, `(let x = 1 in x + 1; 9)` gave `x` to `x + 1` only, and an `in` at the end of a line gave the name to the next line only. If a statement after such an `in` uses an outer binding of the same name, put brackets round the `let ... in` and its expression: `((let x = 1 in x + 1); 9)` (#203).

### Changed

- `in` gives the name to the rest of the body, past a `;`, as in OCaml, Haskell and F#. Brackets narrow a name (#203).
- `wand f` writes `in` after a value that ends on a `match` or `handle` arm in a block of several lines. Before, it put brackets round the `match` (#203).

### Fixed

- An `in` at the end of a line gives the name to the rest of the body. Before, `let y = x in` gave `y` to the next line only (#203).
- `wand f` breaks every stage of a long chain of `+`, `-` or `++`. Before, the last operator could stay on the line of the one before it (#202).
