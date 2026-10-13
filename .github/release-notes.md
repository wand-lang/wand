## 0.107.1 - 2026-10-13

This release fixes one formatter bug. `wand f` removed the brackets from a
`with` that was an operand of an operator, so the operator went into the
`with` body and the program changed.

### Fixed

- `wand f` keeps the brackets on a `with` that is an operand of an operator. Before, the operator went into the `with` body (#179).
