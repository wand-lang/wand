## 0.104.0 - 2026-10-09

This release makes the standard library smaller. A review against every
wand-lang package found names that did the same thing as another name, and
wrappers that added nothing; those are gone. A type now reads itself from
text with `of_string` in its own module. A sum of words also derives the
list of its constructors.

### Upgrading

Each removed name gives a type error that names its replacement.

- `String.to_int s` is now `Int.of_string s`. Every `String.to_*` moves the
  same way: `Float`, `Bool`, `Path`, `Glob`, `URL`, `IPv4`, `CIDR`, `Port`,
  `Version`, `Size`, `DateTime` and `Duration` each have `of_string` and,
  except `Path`, `of_string!`. Add the type's `import`.
- `Shell.decode` and `Shell.lines` are now `Decode.run` and `Decode.lines`.
- `List.append` is now `List.concat`, and `Path.dirname` is now
  `Path.parent`.
- `Duration.add a b` is now `a + b`, and `Duration.sub a b` is now `a - b`.
- `HTTP.header_list` is removed; use `HTTP.header`, which gives an `Option`.
- `Args.parse_with` is removed; use `Args.read` with `T.parser`, where a
  `Bool` field is a switch.
- `Int.divmod a b` is removed; write `(a / b, a % b)`.

### Added

- `of_string` and `of_string!` on `Int`, `Float`, `Bool`, `DateTime`,
  `Duration`, `Size` and `Port`. `Bool` is a new module.
- `Decode.run` and `Decode.lines` read text with a decoder.
- `T.all` lists the constructors of a sum whose constructors hold no value,
  in declaration order (#111).

### Fixed

- A type declared in the REPL or with `-e` has its derived members, such as
  `T.decoder`.
