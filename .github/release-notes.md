## 0.114.3 - 2026-10-26

This release fixes a formatter bug that could change what a file does, when a bracket stands against a constructor's name.

### Fixed

- `wand f` keeps a bracket against a constructor's name, as in `Error("")`. Before, it wrote `Error ""`, so a file with a type error came back as a file without one (#219).
