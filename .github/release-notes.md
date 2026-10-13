## 0.107.0 - 2026-10-12

This release makes the evaluator faster and closes holes in how values go
into a command. On the speed benchmark, each task now takes less than two
times as long as Python. Nine fixes from an audit of the shell quoting
against zx and Bun stop a value from being read as shell source: in
arithmetic, between the author's double quotes, through a raw splice, and
as an option. A `match` arm can have more than one pattern, and
`Regex.capture` gives an `Option`.

### Upgrading

- `HTTP.request` refuses a header name that is not an RFC 7230 token, and a
  header value with a CR, LF or NUL byte. Use a token name, and remove line
  breaks from values (#167).
- A `%{x}` that bash reads as arithmetic (`-eq` and the other number tests
  in `[[ ]]`, `let`, `declare`, `typeset`, `local`) must be an `Int`.
  Convert the value with `Int.of_string` first. A `%{x}` after `-v` or
  inside `( )` in `[[ ]]`, alone in `[[ ]]`, or as an argument of `unset`
  is an error (#169).
- A `%{x}` in the assignment of a variable that the same command reads as
  arithmetic, as in `X=%{y}; echo $((X))`, must be an `Int`. Convert the
  value with `Int.of_string` first (#177).
- A command whose `%!{...}` text opens a quote and leaves it open, or
  closes your quote, is refused. Close each quote in the same `%!{...}`
  text (#170).
- `Shell.in_dir` does not use `CDPATH`, and `-` is a directory named `-`.
  Give the directory that you mean (#172).
- A `Command` in `%{...}` or `%!{...}` is a type error. Run it with
  `Shell.run!` and splice the `String`, or write both in one `$(...)`
  (#174).
- A `Path` that starts with `-` goes in as `./-...` where it starts a word.
  A script that gave such a `Path` as an option must give a `String`
  (#175).
- `Regex.capture` gives an `Option (List String)`. Run `wand t --fix`,
  which adds `|> Option.default []` after an old use and keeps what it did
  (#126).
- A package that uses an arm with more than one pattern must require this
  release in `wand.pkg` (#141).

### Added

- `bench/serve.wand` measures the requests per second of `HTTP.serve!` against two Python servers (#155).
- A `match` arm can have more than one pattern, `| p1 | p2 -> body`, also inside a pattern as `Some (1 | 2)`. A `handle` arm can name more than one operation (#141).
- `V-ARM1` warns about a pattern of an arm that the patterns before it already match (#141).

### Changed

- A pattern finds its constructor one time and keeps it, which makes pattern matching faster (#166).
- The evaluator reads a variable or a module member with less work (#165).
- The evaluator calls a function of one parameter with less work (#164).
- The evaluator does less work at each position in the source (#162).
- `String.words` is faster for a word longer than eight bytes (#163).
- `String.words`, `String.slice`, `String.lines` and `String.split` copy a short piece of text faster (#163).
- A `Command` in `%{...}` or `%!{...}`, also as a list element, is a type error. Before, it gave its source text (#174).
- A `p.N` read can get the type of its tuple from a use further down the file. Before, the type had to come from the definition that reads the element (#154).
- `wand t --fix` changes a bare constructor of a standard library module, such as `Sha256`, to `Digest.Sha256` when only one module has that constructor. When more than one module has it, the error names each one (#153).
- `Regex.capture` gives an `Option (List String)`: `None` when the pattern does not match. Before, it gave `[]` (#126).
- When an old use of `Regex.capture` is a type error, the error says that the value can be `None`, and `wand t --fix` adds `|> Option.default []` after the call. This keeps what the old use did (#126).

### Fixed

- A `%{x}` value between the author's double quotes cannot end the quote in a Big5, GBK or GB18030 locale. The value goes in single-quoted between the two double-quoted parts (#168).
- A `Path` that starts with `-` in `%{...}` at the start of a word, also as a list element, goes in with `./` in front, so that no command reads it as an option (#175).
- `HTTP.request` refuses a header name that is not an RFC 7230 token and a header value with a CR, LF or NUL byte (#167).
- A command longer than the system limit raises an error that `try` catches, and does not stop the script with a fatal error (#171).
- `Shell.in_dir` does not use `CDPATH` and does not read `-` as `$OLDPWD`, so it runs a command in the same directory as `Shell.stream` (#172).
- A command whose `%!{...}` text changes the quoting around a `%{...}` value is refused when it is built. Before, a raw `"` could put the value where the shell read it (#170).
- A `%{x}` value that bash reads as arithmetic, as an operand of `-eq` and the other number tests in `[[ ]]` or as an argument of `let`, `declare`, `typeset` or `local`, must be an `Int`. Before, a value such as `a[$(cmd)]` ran `cmd` (#169).
- A `%{x}` value after `-v` or inside `( )` in `[[ ]]`, alone in `[[ ]]`, or as an argument of `unset` is an error (#169).
- The error for a value that is not an `Int` in arithmetic is at the value, and says why (#169).
- A `\` and a newline inside single quotes in a command stay in the command, as `sh` keeps them. Before, they were removed as a line continuation (#173).
- The check of a command at spawn takes a time in proportion to the length of the command. Before, a value of 900 KB added about 0.1 s, and a value with many `'` characters added minutes (#176).
- A `%{x}` value in the assignment of a variable that the same command reads as arithmetic, as in `X=%{y}; echo $((X))`, must be an `Int`. Before, a value such as `a[$(cmd)]` ran `cmd` (#177).
