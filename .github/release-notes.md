## 0.95.3 - 2026-09-30

### Fixed

- A function in an `and` group can call a member defined after it, and a member that does not fit names its line (#65).
- `Wand.check_at` follows `..` out of a directory that does not exist yet (#66).
- V-BANG1 no longer warns about a function that only returns a function that can raise (#67).
