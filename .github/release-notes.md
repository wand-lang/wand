## 0.110.0 - 2026-10-20

This release makes the space after a constructor say what a bracket is, and fixes three fuzz findings in constructor brackets. One change can make an existing file fail: read Upgrading.

### Upgrading

- A bracket against a constructor's name is now its field list, and only a constructor with named fields has one. `Some(x)`, `Pair(a, b)`, `Rect(1, 2)`, `Ok()` and `Red()` are errors, and so are `Pod (name = "a")` and the declaration `type Pod (name: String)` with a space, and `type W = Wrap(List Int)` without one. Run `wand t --fix`, which writes `Some x`, `Pair (a, b)`, `Rect 1 2`, `Ok ()`, `Red`, `Pod(name = "a")`, `type Pod(name: String)` and `Wrap (List Int)` (#207).

### Changed

- A bracket against a constructor's name is its field list, for a constructor with named fields only. A bracket after a space is an ordinary argument. This holds in a construction, a pattern, an update and a declaration. Each wrong spelling is an error with a fix (#207).
- `wand f` writes a tuple pattern and a qualified constructor's payload after a space: `Some (k, _)`, `d.M (N)` (#207).

### Fixed

- A pattern reads a bracketed first argument with more arguments after it: `Rect (Circle r) h`. Before, it was a parse error, and `wand f` could write a qualified constructor so that the file no longer parsed (#204, #205, #206).
- A declaration takes a bracketed field with more fields after it: `type R = Rect (List Int) Int` (#204).
- `P (1, 2) 3`, where the first field of `P` is a pair, builds a `P`. Before, it was refused with the correction for `Rect (3, 4)` (#204).

### Documentation

- The reference has a section on a bracket against a constructor, and its examples write named fields against the name (#207).
