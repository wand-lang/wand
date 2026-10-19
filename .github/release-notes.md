## 0.107.5 - 2026-10-19

This release fixes one bug in `wand f` that the fuzzer found.

### Fixed

- `wand f` keeps a constructor applied to a bare name or a tuple of bare names as an application. Before, `f T(m;)` became `f T (m)`, which reads as the construction `T(m = m)` (#186).
