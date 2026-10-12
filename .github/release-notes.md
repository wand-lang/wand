## 0.105.0 - 2026-10-12

This release comes from the authoring benchmark. Each change removes an error
that an agent met while it wrote a script. Where wand knows the one correct
form, it now accepts that form, or `wand t --fix` writes it. A tuple element
is `p.0`, a match guard is `if`, `++` joins lists, a map literal takes puns,
and a type converts with `T.to_string`. `wand t --fix` now adds every missing
import, changes names from other languages to the wand names, and wraps a
value in `T.to_string` where a `String` is expected.

### Upgrading

- `String.of_int` is removed. Run `wand t --fix`, which changes it to
  `Int.to_string` (#143).
- A string in JSON, TOML or YAML no longer decodes as an `Int`, `Float` or
  `Bool`. Decode the field as a `String` and read it with `Int.of_string`,
  or correct the data (#125).
- `*`, `**` and `?` in a glob now match names that start with a dot, so
  `./**/*` gives `.env` and the files under `.git/`. Filter the results
  where you do not want them (#138).
- An arm that starts a line to the left of a nested `match`'s first arm now
  belongs to the outer `match`. If `wand t` reports a match that is not
  exhaustive, indent the arm under the inner `match` (#135).
- `"%{f}"` writes a `Float` with all the digits needed to read it back:
  `0.1 + 0.2` is `0.30000000000000004`. Use `Float.round` where the output
  is compared (#143).
- `wand f` writes a `when` guard as `if`, which an earlier wand cannot read.
  A package that uses `if` guards, `p.0`, `[a, b :: rest]`, `{a}` in a map
  literal or `++` on lists must require this release in `wand.pkg`.

### Added

- `p.0`, `p.1` and so on read an element of a tuple: `("a", 1).1` is `1`.
  The index must be below the tuple's length (#144).
- `p.0` is accepted when the tuple's type is known later in the same
  definition, as in a fold step (#147).
- A match guard can start with `if`: `| Ok n if n >= 1 -> n` (#142).
- `++` joins two lists as well as two strings: `[1] ++ [2]` is `[1, 2]`
  (#151).
- A name alone in a `{...}` map literal puns: `{a, b = x}` is
  `{a = a, b = x}` (#146).
- `[a, b :: rest]` in a pattern is `[a :: b :: rest]` (#117).
- `.*.txt` is a glob literal, as `*.txt` is (#138).
- `to_string` on `Int`, `Float`, `Bool`, `Duration`, `Size`, `DateTime` and
  `Port` (#143).
- `List.flat_map` (#132).
- `List.contains?` (#136).
- `Path.relative_to` and `Path.relative_to!` (#133).
- A `;` at the start of a line in a body has the meaning of a `;` at the end
  of the line above (#152).

### Changed

- `wand t --fix` adds every missing standard library import in one run,
  also when the file has an error that it cannot fix (#150).
- `wand t` reports every missing standard library import with the first
  error (#150).
- `wand t --fix` changes a name from another language to the wand name:
  `List.iter` becomes `List.each`, `Err` becomes `Error` (#140).
- `wand t --fix` changes a member name that differs only by a last `?` or
  `!`: `List.contains` becomes `List.contains?` (#140).
- `wand t --fix` puts `T.to_string` around a value of type `T` where a
  `String` is expected (#143).
- `wand t --fix` adds `|> Option.default 0` after `List.sum` where its
  element type is expected (#127).
- `wand t --fix` writes the type on a parameter that is read only through a
  field: `let f (r: ShellResult) = r.stdout` (#139).
- `wand t --fix` changes a `:` written for cons in a pattern to `::` (#117).
- `wand t --fix` changes `let {X} = Digest` to `let {X} = import Digest`
  (#149).
- `wand t --fix` makes up to 50 passes, not 5 (#140).
- `wand f` writes a `when` guard as `if` (#142).
- `wand f` writes `{a = a}` in a map literal as `{a}` (#146).
- A `;` at the end of a line in a body continues the body, as a newline
  does (#131).
- A `then` or `else` branch that starts on a line of its own holds one
  statement per line (#137).
- `*`, `**` and `?` in a glob match names that start with a dot (#138).
- A string in JSON, TOML or YAML does not decode as a number or a `Bool`,
  and the error says `expected Int, got String "12"` (#125).
- `"%{f}"` writes a `Float` with all the digits needed to read it back
  (#143).
- `result` is an ordinary name outside an `ensures` clause (#128).
- A non-exhaustive match says which arm to add, written as a person writes
  it: `[_ :: _ :: _]` (#134).
- A map literal whose values have different types says that fields of
  different types need a `type` (#146).
- `xs[1]` says to write `List.get 1 xs` or `List.get! 1 xs` (#145).
- `List.sum`, `List.max` and `List.min` where their element type is
  expected say that they give an `Option` (#127).
- A name from another language, such as `List.mapi`, gives a hint that
  names the wand function (#136).
- `IO.eprintln` gives the hint `IO.println_err`, and `List.nth` gives
  `List.get` (#130).
- A reserved word where a name goes says that it is reserved (#128).

### Removed

- `String.of_int`. Use `Int.to_string` (#143).

### Fixed

- `Digest.Sha256` with no `import Digest` gives the missing-import error,
  not an error that printed the internal form of the expression (#148).
- `let {Sha256} = Digest` gives the missing-import error at its position
  (#149).
- The `|>` hint is added only where two types do not fit (#148).
- A failed `requires` or `ensures` clause is written as in the source
  (#148).
- A type error in a `match` pattern shows the position of that pattern
  (#129).
- The editor puts an inserted import where `wand t --fix` puts it, not on
  the first line (#150).

### Documentation

- The reference writes the chained cons pattern with `::`, not `:` (#117).
