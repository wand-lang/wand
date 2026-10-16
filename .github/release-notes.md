## 0.107.4 - 2026-10-16

This release fixes one bug in `wand f` that the fuzzer found.

### Fixed

- `wand f` keeps the `;` after an item when a comment comes between it and a line that starts with an operator. Before, the line below joined the item, so `let {} = import s;  --` above `- ...` became `import s - ...` (#185).
