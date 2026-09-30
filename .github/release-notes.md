## 0.95.0 - 2026-09-30

### Upgrading

- A file that stores a command-running function in a record field must now declare the effect. `wand t` names the line.
- `Shared.update` returns a value: drop it where `Unit` is required.

### Added

- `\xNN` byte escapes in strings and in regex character classes (#56).
- `wand f` takes a directory (#57).
- `Wand.check_at`: check text as the file at a path (#59).
- `Checked.effects`: what a source performs (#60).

### Changed

- `Shared.update` returns the value from before the update (#55).

### Fixed

- A function stored in a record field is charged where it is stored (#61).
- A record update uses the type of its own module (#50).
- An interface can name its own module's types across modules (#51).
- A list can hold different modules that implement one interface (#52).
- A continuation used after its case answered is an error, not a crash (#53).
- A self-call inside a closure no longer takes on the closure's effects (#54).

### Documentation

- Serving fake connections in a test (#58).
