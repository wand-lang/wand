## 0.115.0 - 2026-10-30

This release fixes a formatter bug that could change what a file does, when a bare `(import Regex)` stands at the top level, and adds a fix for that type error.

### Added

- `wand t --fix` turns a bare `(import Regex)` at the top level into the plain import `import Regex` (#220).

### Fixed

- `wand f` no longer turns a bare `(import Regex)` at the top level into the plain import `import Regex`, which changes what the file means (#220).
