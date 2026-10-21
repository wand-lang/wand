## 0.113.0 - 2026-10-21

This release merges `Digest` into `Hash`, makes `fun` and `when` errors with a fix, and lets `wand d` show a type. A file that uses `Digest`, `fun`, `when` or a `_` in a field list fails until `wand t --fix` runs.

### Added

- `wand d` shows a type: its declaration, with its fields or its constructors, and its doc. A constructor shows the type it belongs to (#213).

### Changed

- `Digest` is merged into `Hash`, and its type is `Hash`: `Hash.Sha256`, `Hash.hex`, `Hash.of_hex`. `Digest` is an error, and `wand t --fix` rewrites every use in one run (#215).
- `fun` and `when` are errors, and `wand t --fix` writes `fn` and `if` (#212).
- `for` and `do` say that wand has no loops, and name `List.each` and `List.map` (#212).
- A `_` in a field list, as in `Pod(name, _)`, is an error, and `wand t --fix` removes it. A field list ignores the fields it does not name (#214).
- A name that is not a field lists the type's fields, and `r.exit_code` suggests `code`, with a fix (#214).

### Fixed

- `wand f` brackets a `match` subject that takes more than one line. Before, an `if` there could become source that did not parse (#211).
- A field list with a `_` reported a wrong argument count, even when every field name was correct (#214).

### Documentation

- The reference's reserved words say what each word that starts no form does (#212).
