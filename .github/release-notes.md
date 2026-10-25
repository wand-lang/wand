## 0.114.2 - 2026-10-25

This release fixes a formatter bug that could change what a file does, when an item follows an item with a comment inside it on the same line.

### Fixed

- `wand f` keeps an item as written when it follows an item with a comment inside it on the same line. Before, a second run could split it into separate items (#218).
