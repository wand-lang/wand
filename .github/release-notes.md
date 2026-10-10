## 0.103.0 - 2026-10-09

This release is the result of a security review of the whole codebase. Most
of it is hardening: the manifest and the `%{}` quoting now hold in shell
forms that could slip past them before, packages are fetched and read more
carefully, and a number of stdlib edge cases that returned a wrong value now
return an error. Several of these change behaviour a file may rely on; see
Upgrading.

### Upgrading

- `CSV.parse` and `CSV.parse_with` now answer a `Result` rather than a bare
  list. Match the `Result`, or call the new `CSV.parse!` / `CSV.parse_with!`.
- A `%{}` splice inside backticks, a `$'...'` string, or a heredoc body is
  now a parse error: no quoting there makes the value one argument. Splice it
  outside the quotes, use `$(...)` in place of backticks, or pipe with `|>`.
- A `%{}` splice in `$(( ))` arithmetic must now be an `Int`.
- A file that narrows `Shell(...)` can no longer use `[[ ... ]]`; write the
  comparison in wand, or declare bare `Shell`.
- A negative count given to `Duration.seconds`, `scale` and the rest, and to
  `Size.of_bytes`, now raises rather than being made positive or `0`.
- `Float.round`, `floor` and `ceil` now raise for NaN, infinity and a value
  outside the `Int` range rather than answering `0`.
- `String.to_int` now reads decimal digits only, not `0x1F`, `1_000` or `+5`.
- A date or time that does not exist (`2024-02-30`, `25:00:00Z`) is now an
  error where it was read, literal or `String.to_datetime`.
- `Base64.decode`, `CIDR.of_string` and `URL.of_string`/`with_hostname` now
  reject input they used to accept; `**/` in a glob now matches a file at the
  top level too.
- An interface member written with no effects is now pure, so a module that
  performs effects can no longer implement it; write the effects on the
  member.

### Added

- `Net.listen_on` serves on one address.
- `!` siblings for `URL.of_string`, `URL.join`, `URL.decode`,
  `IPv4.of_string`, `CIDR.of_string`, `Version.of_string`, `Glob.of_string`
  and `Regex.compile`.
- `CSV.parse!` and `CSV.parse_with!`.

### Changed

- A run checks a script before its imports run, and runs them inside the
  rehearsal, so `--dry-run` withholds an import's work and `wand script.wand`
  checks what `wand t` checks.
- `wand d --load` and `wand t -e --load` read a file without running it.
- The language server fetches no packages and stops a check after 5 seconds.
- `Random.shuffle` sorts by random keys, and `List.map`/`filter`/`length`
  and `String.join` run on inputs of any length.

### Fixed

- Security: a `%{}` value is quoted for the shell context it lands in and
  refused where none is safe; the command-word scan finds commands hidden in
  arithmetic, `$'...'`, `#` comments and process substitution; a command word
  a substitution makes, and the words an import runs as it loads, are checked
  against `Shell(...)`.
- Security: a fetched package with a symbolic link or a `..` path is refused,
  and `wand t` and the language server no longer clone from a repository's
  own `wand.pkg`.
- Security: an HTTP redirect to another origin drops credential headers, curl
  runs with `-q` and `--globoff`, and `HTTP.serve` refuses ambiguous body
  framing and line breaks in headers.
- Security: `--dry-run` withholds `Net.listen` and `Net.write`,
  `FS.write_atomic` keeps its temp file private, and `FS.copy`/`copy_tree`
  refuse a copy onto or into the source.
- A connection ends at a line over 1MB or 60 seconds idle, and a stream
  stopped early ends its whole command.
- `Random.int` takes any range, and `Par.map` no longer hangs on an OCaml
  exception.
- `JSON.parse` keeps an integer past the `Int` range as its digits; a `Size`
  too large for an `Int` raises.
- A dry run reads a withheld write under any spelling of its path.
- `--strict` and `--lint` work before a script path, `wand i` refuses an
  unknown option, the "did you mean" hint is safe to paste, and the timeout
  watchdog retries an interrupted wait.
- `Version.bump_minor` and `bump_major` bump a prerelease to its release.
- `check_fmt` and `check_docs` fail when they find nothing to check.

### Documentation

- The reference describes how `%{}` is quoted in each shell context.
- `HTTP.header_list`'s doc says it answers one value; the `verify-archives`
  and `provision-host` examples do what their comments say.
