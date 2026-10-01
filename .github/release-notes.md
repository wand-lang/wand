## 0.99.0 - 2026-09-30

### Added

- `fuzz --eval` runs each program it generates, and checks that it performs only the effects its type names; the scheduled fuzz job runs it (#73).
- `Shared.wait`: wait until a `Shared`'s value passes a test, woken by the update that makes it pass (#77).
- A lambda given for a field takes its parameter types from the field (#76).

### Fixed

- A field default may be a constructor reached through a module (#74).
- `List.take` and `List.drop` with a count below 0 take none and drop none (#75).
- A message names an imported type as the file writes it, as `M.P` (#76).

