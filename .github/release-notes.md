## 0.93.0 - 2026-09-29

wand's own tools as functions, commands in another directory, and
manifests that name the commands of what a script calls.

### Upgrading

Three changes can make a file fail that passed with 0.92.0. `wand t`
names each place, and `wand t --fix` corrects the first two:

- A `Shell(...)` list must now hold the commands of the imported
  functions that the file calls. `--fix` adds the missing words.
- `V-DROP3` reports a statement whose value is a function, such as
  `main!` with no `()`. `--fix` adds the `()` to a bare `main!`. For a
  call that is short of an argument, give it the argument.
- The lines under a `match` or `handle` arm are statements now. An arm
  that wrote one call over two lines at the same column must indent the
  second line further.

### Added

- **A `Wand` module: `wand f` and `wand t` as functions.** A program that
  writes wand can format its output as `wand f` does, and a program that
  checks wand files gets the answer as values, not as JSON text:

  ```
  FS.write_file! path (Wand.format! text)
  (Wand.check_file! ./deploy.wand).diagnostics
  (Wand.check "import List\nList.fold_left ? 0 [1, 2, 3]").holes
  ```

  `format` and `format!` format source. The result is what `wand f`
  writes, so a code generator no longer has to copy its layout rules.
  `check` checks source text, which can be one expression or a whole
  program. It gives the errors and lint warnings, each `?` and its type,
  and the type of the source. `check_file` and `check_file!` check a file
  with what it imports, as `wand t` does. `version` is the version of the
  wand that runs the script.

  `check` reads no files, so it can import the standard library and
  nothing else. None of these functions runs the source it is given.

- **Run a command in another directory.** `Shell.in_dir dir c` is the
  command `c`, to run in `dir`. Give it to `Shell.run!`, `inspect!`,
  `inspect_with!`, `query`, `stream`, `spawn` or any other function that
  takes a `Command`:

  ```
  Shell.run! (Shell.in_dir copy $*(wand cli.wand gen apis/example.com/v1alpha1))
  json |> Shell.inspect_with! (Shell.in_dir repo $*(kubectl apply -f -))
  ```

  Before, the only way was `$(sh -c 'cd "$1" && cmd' ...)`. That put `sh`
  in the manifest, which allows any command, and `wand t` could not check
  the commands inside the shell text. Now the manifest names the
  command's own words.

  Only that command runs in `dir`. The script and its other commands stay
  where they are. A second `in_dir` with a relative directory is read from
  the first, as a second `cd` is. A directory that is not there raises
  when the command runs. `--dry-run` shows the directory.

- **A warning for a function that nothing calls (`V-DROP3`).** A call
  that is short of an argument makes a function, not an error. As a
  statement, that function does nothing, and before, nothing said so:

  ```
  let count! n =
    log! "found"      -- log! takes a label and an Int; this line does nothing
    n
  ```

  The same rule finds a script that ends with `main!` and not `main! ()`,
  which runs nothing. `wand t --fix` adds the `()`. `wand t --expr` and the
  REPL do not report the expression you ask about.

- **`List.empty` and `Map.empty?`.** Now each of `List` and `Map` has the
  empty value and the test for it. `List.empty` is `[]`, and
  `Map.empty? m` is true when `m` has no entries:

  ```
  Map.empty? {}        -- true
  Map.empty? {a = 1}   -- false
  ```

### Changed

- **A script can name the commands that its imported functions run.**
  A call runs the commands of the function it calls. So a script that
  calls `plimsoll.apply!`, which runs `kubectl`, can now say so on its
  first line:

  ```
  uses {IO, Shell(kubectl)}
  ```

  Before, `wand t` said that `kubectl` was not used, and `--fix` changed
  the line to bare `Shell`, which allows any command. Only the functions
  that the script names count. A function that it does not call adds
  nothing, so you do not copy a module's manifest into each script.

  A word that a called function runs and the list omits is now a type
  error, and `wand t --fix` adds it. A suggested manifest includes these
  words. If a called function can run any command, because its module has
  bare `Shell` and the command name is not written out, a narrow list
  cannot hold it: the error tells you to declare bare `Shell`, and
  `--fix` does that.

  This can make a file that passed before fail. `--fix` corrects it.

- **Each line of a `match` arm is a statement, as in a function body.**
  Two lines under an arm now run one after the other:

  ```
  | Some n ->
    IO.println "found %{n}"
    n
  ```

  Before, an arm read these lines as one call, the first line applied to
  the second. When that call did not typecheck, the error told you to use
  brackets. When it did typecheck, the arm did something different from
  what it showed, with no error. The same lines in a function body were
  already two statements, so the two now agree. `handle` arms follow the
  same rule. `wand f` writes the lines as `(IO.println "found %{n}"; n)`.

  An arm that wrote one call over two lines at the same column now reads
  them as two statements. Indent the second line further to continue the
  call.

- **Four errors now tell you what to write.** Before, each one gave only
  the types:

  - A name used above the line that defines it:
    `'g' is defined below, at line 2. Move the definition above its first use`.
    Before, the error said the name was unbound and suggested a different
    name.
  - `|>` after a call, as in `List.length xs |> List.map f`, pipes the
    whole call, not its last argument. The type error now says so, and
    shows the brackets that pipe one argument: `f a (b |> g)`.
  - A field default that is not a literal, such as
    `env: Map String = Map.empty`. The error now adds
    ``An empty map is `{}` `` (or ``An empty list is `[]` ``).
  - A value piped into `Shell.inspect!`, which takes no input. The error
    now says to write `x |> Shell.inspect_with! $*(...)`.

  `wand t --fix` corrects two of them. It changes a default of
  `Map.empty` to `{}` and `List.empty` to `[]`, and
  `x |> Shell.inspect! cmd` to `x |> Shell.inspect_with! cmd`. The other
  two need a decision from you, so `--fix` leaves them.

- **An error in one stage of a `|>` pipeline points at that stage,** not
  at the start of the pipeline.

### Fixed

- **`wand f` no longer breaks a function that a value shadows.** A
  function reads each `let` of the same name below it as one more
  equation, and only a `;` stops that. `wand f` put the two on separate
  lines and removed the `;`:

  ```
  let f i = i; let f = 3
  ```

  The result did not parse. Now `wand f` keeps the `;`:

  ```
  let f i = i;
  let f = 3
  ```

- **A recursive glob can be written in brackets.** `FS.glob (**.wand)` was
  a lex error that said "a comment is `--` to the end of the line",
  because wand read `(**` as the start of an OCaml doc comment. You had to
  write `( **.wand)`, and `wand f` then took the space out and wrote a file
  that did not lex. Now `(**` starts that error only when a space follows
  it, as in `(** doc *)`, so a glob just inside a bracket is a glob:

  ```
  FS.glob (**.wand)
  ```

- **`wand f` opens a `fn`'s block on the `fn` line.** A `fn` whose body
  is a block of statements put the block's `(` on a line of its own, with
  the statements two more columns in. Now the block opens on the `fn`
  line and closes at the `fn`'s indent, as a block does after `=` and
  `then`:

  ```
  Par.map
    8
    (fn f -> (
      let digest = Hash.file! Digest.Sha256 f |> Digest.hex;
      Shared.update done (fn n -> n + 1);
      (f, digest)
    ))
    files
  ```

- **`wand f` gives the same result each time for `let ... in` before an
  empty `;`.** In `(let x = 1 in x;)`, wand read the `;` as keeping `x`
  from the statements after it, but no statement follows. `wand f` wrote
  `(let x = 1 in x)`, and the next `wand f` changed that to
  `(let x = 1; x)`. Now a `;` with nothing after it changes nothing, and
  the first pass writes `(let x = 1; x)`.
