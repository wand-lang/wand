## 0.114.1 - 2026-10-24

This release fixes a formatter bug that could change what a file does, when an item with a comment inside it ends with a `;` and the next item starts on the same line.

### Fixed

- `wand f` keeps an item on the line after a `;` when the item above it has a comment inside it. Before, the item could join the body above and change what the file does (#217).
