## 0.107.6 - 2026-10-19

This release fixes four bugs and one broken link in the reference.

### Fixed

- `TOML.get_table`, `TOML.decode` and `TOML.stringify` keep a table's keys in the order the document wrote them, as JSON and YAML do. Before, they sorted them (#192).
- A quoted TOML key, such as `"a.b"`, is read without its quotes, and `TOML.stringify` writes an empty table. Before, the key kept its quotes and the empty table was left out (#192).
- `fn Entry(name) -> name` puns the field `name`, as a `let` and a `match` pattern do. Before, it was read as two parameters, `Entry` and `(name)` (#191).
- `wand f` writes a body of statements one per line as one flat block. Before, three statements came back as `((a; b); c)` (#187).
- `wand f` breaks a chain of `+`, `-` or `++` that does not fit, one operator per line. Before, it joined the chain onto one line past the margin (#188).

### Documentation

- The link to "A command as a value" in the reference's `Shell` section goes to that heading (#189).
