## 0.115.1 - 2026-11-08

This release fixes a formatter bug that wrote a file that does not parse, when a contract stands on a binding in a block.

### Fixed

- `wand f` keeps the indent on the body below a contract in a block, as in `let t : Int = ensures result > 0`. Before, it moved the body back to the column of the `let`, so the file did not parse (#222).
