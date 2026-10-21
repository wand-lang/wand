## 0.113.1 - 2026-10-21

This release fixes the 0.113.0 formatter, which wrote `match ((a, b)) with` for a tuple subject that takes more than one line.

### Fixed

- `wand f` no longer adds a second pair of brackets to a tuple, list or map `match` subject that takes more than one line (#211).
