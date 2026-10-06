## 0.102.0 - 2026-10-06

### Upgrading

- `unless` is now a keyword. Rename a name or a map key spelled `unless`, or put the key in quotes: `{"unless" = 1}`.

### Added

- `unless c then a else b`, which is `if` with the condition the other way. The `else` is optional, as it is for `if`.
- `A-IF1` reports a `match` with only the arms `true` and `false`, and says whether `if` or `unless` fits.

### Documentation

- The reference describes `unless` and `A-IF1`.
