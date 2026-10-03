## 0.101.2 - 2026-10-03

### Fixed

- A constructor or a derived member, as `Line.usage`, runs when it is used above its type's `type` line. It typechecked, and then the run failed with "unknown constructor" (#82).
