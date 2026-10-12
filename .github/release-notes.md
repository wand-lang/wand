## 0.106.0 - 2026-10-12

This release makes wand faster. On the speed benchmark, the task
`03-slowest` is about four times as fast as in 0.105.0, and `02-top-ips` and
`09-distinct` are about two times as fast. wand is faster than Python on six
of the fifteen tasks. The evaluator finds each variable by its position,
reads the task state less often, reads stream lines in blocks, and makes
fewer string copies. A list in `%{xs}` gives one command argument for each
element. `List.smallest_by` and `List.greatest_by` give the first `n`
elements by a key, without a sort.

### Upgrading

- `%{xs}` with a `List` in a command gives one argument for each element,
  not one argument that holds the text of the list. To keep one argument,
  join the list first: `%{String.join " " xs}` (#113).
- A `List` in `%!{...}` is a type error. Run `wand t --fix`, which changes
  it to `%{...}` (#113).
- A list of lists in `%{...}`, or a list between your own quotes in a
  command, is a type error. Join the list to a `String` first (#113).
- `List.sort_by` with a key function that has effects now has those effects
  in its type. Run `wand t --fix` to add them to the manifest (#121).
- A package that uses `Run(a)` with one name, `Ok ()` as a pattern, a list
  in `%{...}`, `List.smallest_by` or `List.greatest_by` must require this
  release in `wand.pkg`.

### Added

- One name alone in a constructor's brackets puns, in a pattern and in a construction: `Run(a)` is `Run(a = a)` when `a` is a field of `Run` (#115).
- `Ok ()`, `Some ()`, `Error ()` and a constructor of your own whose payload is `Unit` match `()` as the payload: `| Ok () -> ...` (#116).
- `%{xs}` in a command with a `List` gives one quoted argument for each element, as `"$@"` does, and an empty list gives no argument (#113).
- A list that holds lists in `%{...}` is a type error (#113).
- A list in `%{...}` between the author's own quotes is a type error (#113).
- A list in `%!{...}` is a type error, and `wand t --fix` changes the `%!{` to `%{` (#113).
- `wand t` reports a `%!{x}` in a command where `x` is not a `String`, such as a `Path`, as `V-SHELL4`, and `wand t --fix` changes it to `%{x}` (#123).
- `List.smallest_by n f xs` gives the `n` elements with the smallest keys, smallest first, without a sort of the whole list. It gives the same answer as `List.sort_by f xs |> List.take n` (#121).
- `List.greatest_by n f xs` gives the `n` elements with the greatest keys, greatest first. Elements with equal keys keep their input order (#121).
- `Stream.smallest_by` and `Stream.greatest_by` do the same over a stream. They read the stream once and keep only `n` elements (#121).
- `wand t --fix` changes `List.nsmallest`, `List.least_by` and `List.k_smallest` to `List.smallest_by`, and `List.nlargest`, `List.largest_by` and `List.k_largest` to `List.greatest_by`. The same names on `Stream` change to the `Stream` functions (#121).
- `List.top`, `List.min_by` and `List.max_by`, and the same names on `Stream`, give a hint that names `smallest_by` or `greatest_by` (#121).

### Changed

- Stream lines are read in blocks, which makes line streams faster (#158).
- A `;` at the end of a line inside a `( ... )` block continues an arm or branch body when the next line is at the body's column, as a newline does (#119).
- In a construction of a record of one field, one name that is not the field fills that field: `B(x)` is `B(n = x)` (#115).
- `wand f` keeps one name in a constructor's brackets as it is written, `Some(x)`, and no longer writes it as `Some x` (#115).
- The benchmark harness gives a case's arguments to the program with one list splice, and takes any number of them (#113).
- The benchmark harness writes its long `claude -p` command over several lines (#114).
- The speed benchmark task `03-slowest` uses `Stream.smallest_by` and does not sort all the lines (#121).
- `String.words` is faster (#122).
- `String.split` with a delimiter of one character is faster (#122).
- `String.lines` is faster (#122).
- `String.words` does not copy a text that is one word (#160).
- `String.slice` is faster (#160).
- `String.starts_with?` is faster (#160).
- `Int.of_string` is faster (#160).
- `Stream.unique` over a stream of `String` is faster (#122).
- A stream with `map`, `filter` or `filter_map` stages is faster for each element (#122).
- A decoder derived from a type, as `JSON.decode Event.decoder`, is faster (#122).
- `List.map`, `filter`, `filter_map`, `flat_map`, `fold_left`, `fold_right`, `find`, `any?`, `all?`, `take_while`, `drop_while`, `each`, `contains?`, `indexed`, `zip` and `take` are builtins. `List.find` over a long list is approximately seven times faster, and it calls the function on the same elements in the same order (#122).
- A call to a wand function is approximately 15% faster (#122).
- Each step of evaluation is cheaper. A loop that calls a function is approximately 25% faster, and a program that runs for ever still stops at once on Ctrl-C (#157).
- A name that a function binds, such as a parameter or a `let`, is found by its place, and a name from outside the function is found once and kept. Speed tasks 02, 03 and 09 are approximately 30% faster (#156).
- A builtin that calls a function for each element, such as `List.fold_left`, is faster (#159).
- A call that gives two arguments to a function of two parameters, as in a fold or after `|>`, is faster (#159).

### Fixed

- A function value bound by `let`, such as `let inc = add 1`, no longer sees its own new name inside the function it came from. A program where that name hid another one now runs as it typechecks (#156).
- A constructor used as a function value, such as `Stream.filter_map Some`, is accepted over a stream that reads a file, as a lambda is (#120).
- An effect mismatch on the left of a `|>` no longer gives the hint about `|>` taking the whole call (#120).
- An unbound name that the same definition binds in another scope gives "'x' is bound at line N, but that binding is not in scope here", not the `wand d` hint (#119).
- An error in a constructor pattern has a line and a column in a parameter and in a `let`, as it has in a `match` arm (#115).
- An error in a constructor pattern inside another pattern points at the inner constructor (#115).
- `wand f` keeps `Ok ()` with its space, and puts it in brackets where it is an argument: `f (Ok ()) x` (#116).
- `wand f` keeps each `\` line continuation in a command and its line break, and indents the continued lines (#114).
- `wand f` writes a command that opens on the line under `$?(` with the `)` under the line that opens it, not to the left of the command (#114).
- V-SHELL2 does not report a newline before the first word or after the last word of a command (#114).
- `V-SHELL1` says that a `%!{...}` splice can add words or commands, not that the first word is decided at run time, when the first word is a literal (#123).
- The effects of the key function of `List.sort_by` are in the type of the call. Before, `List.sort_by` with a key that prints typechecked as pure (#121).
- `List.contains?` on a list of more than 1,000,000 elements no longer stops with "too many nested calls" (#122).

### Documentation

- The reference says that one name alone puns (#115).
- The reference shows `| Ok () ->` as a pattern (#116).
- The reference tells how a list goes into a command (#113).
- The reference tells how `wand f` writes a command that continues over lines (#114).
- The reference and the `Shell.in_dir` doc say that a command, and a relative `dir`, use the current directory of the process, not the directory of the script (#118).
