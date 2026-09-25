# Changelog

## [0.80.7] - 2026-09-25

### Changed

- **A contract makes a function raise.** A failed `requires` or `ensures`
  raises, and `try` catches it, but the effects said otherwise. The
  signature read `Int -> Int`, and `V-BANG2` called the `!` on such a
  function a promise of a risk that was not there. A contract now performs
  `Raise`, like everything else that can raise.

  ```
  let half n =
    requires n % 2 == 0
    n / 2

  -- before    half : Int -> Int
  -- now       half : Int -> Int ! {Raise}
  --           V-BANG1: 'half' can raise -- call it 'half!'
  ```

  A caller of a contract-carrying function performs `Raise` as well, so the
  names above it take the `!` too. `wand t --effects` is unchanged: it names
  the labels a manifest would, and `Raise` is not one of them.

### Fixed

- **`wand t --fix` deleted an import the file needed.** `V-IMP2` asked
  whether a name was mentioned in type position, not how often, so a module
  named once as a built-in type counted as unused however many times it was
  also called. `--fix` then deleted the import, hit the type error it had
  just made, reported that it had put the import back, and left the file
  without it.

  ```
  import IO
  import Path

  let show ((p, n): (Path, Int)) = IO.println "%{n} %{Path.to_string p}"

  -- before    V-IMP2: nothing in this file uses Path
  --           and --fix removed the line, leaving a file that does not
  --           typecheck
  -- now       silent; Path is called as well as named
  ```

  A module named *only* as a built-in type is still reported, which is what
  the rule is for.

- **A contract clause could not call a function.** `requires` and `ensures`
  read an expression with no application in it, so a call was cut in two:
  the name became the whole condition, and its argument started the body.

  ```
  let half n =
    requires even? n
    n / 2

  -- before    type error: expected Bool, got Int -> Bool
  --           the condition was `even?` and the body was `n`
  -- now       the condition is `even? n`, the body is `n / 2`
  ```

  `result` can be an argument now as well, so
  `ensures String.length result > 0` says what it looks like it says.

- **`wand f` wrote a lambda's contracts outside the lambda.** The first
  clause was hugged onto the `fn ... ->` line and the rest were written at
  the indent around the lambda, so the indentation said the body belonged
  to whatever held it.

  ```
  -- before
  let pair =
    fn a b -> requires a > 0
    requires b > 0
    a + b

  -- now
  let pair =
    fn a b ->
      requires a > 0
      requires b > 0
      a + b
  ```

## [0.80.6] - 2026-09-21

### Fixed

- **A brace inside a string in `%{...}` did not lex.** The interpolation
  ended at the first `}` that balanced the braces counted since it opened,
  and a brace written inside a string was counted with them. So a `{` in a
  string ran off the end of the file, and a `}` ended the interpolation one
  character into the argument.

  ```
  "%{String.replace "{" "[" s}"

  -- before    lex error: unterminated string interpolation
  -- now       a[b}
  ```

- **`wand f` left a list of punned fields on one line however long it was.**
  A construction whose fields all pun is written as a list of bare names,
  and that form had no wrapped shape. The named spelling of the same
  construction did wrap, so reformatting it twice gave two answers.

## [0.80.5] - 2026-09-19

### Fixed

- **`FS.glob` answered nothing for an absolute pattern.** The walk always
  started at the working directory, so a pattern that names its own
  directory could never match. It came back empty and said nothing.

  ```
  FS.glob /var/log/*.log

  -- before    -- now
  []           [/var/log/displaypolicyd.stdout.log, /var/log/fsck_apfs.log]
  ```

  An absolute pattern now starts its walk where the pattern says.
  `FS.glob_in` is unchanged: it always searches the directory given to it,
  and an absolute pattern there simply matches whole paths.

## [0.80.4] - 2026-09-19

### Fixed

- **A positional payload reached through a lowercase module was dropped by
  `wand f`.** A module bound by `let l = import ./lib` has a lowercase name,
  so `l.S` opens with a word the parser also reads as a variable. The
  bracketed spelling read it; the unbracketed one `wand f` writes did
  not, and the payload became a statement below the constructor.

  ```
  -- type T(l.S) formatted
  -- before        -- now
  type T = T       type T = T l.S
  l.S
  ```

## [0.80.3] - 2026-09-18

### Changed

- **`wand f` no longer brackets a `match` that stands as a statement.** The
  parentheses were for the reader, not the parser, and they made a `match`
  in the middle of a block look unlike the `match` that ends one.

  ```
  -- before                     -- now
  (match same with              match same with
   | false -> stop ()           | false -> stop ()
   | true -> ());               | true -> ();
  ```

  Reformated `tools/fuzz_sweep.wand`.

## [0.80.2] - 2026-09-18

### Fixed

- **A pipeline indented a stage that wrapped two spaces too deep.** The
  closing bracket did not line up with the one it opened.

  ```
  -- before                      -- now
    [                              [
        (r.draft, "a draft"),        (r.draft, "a draft"),
        (bad, "below the floor")     (bad, "below the floor")
      ]                            ]
      |> List.filter_map f         |> List.filter_map f
  ```

  Every stage now starts at the column the pipeline starts at. This
  reformats wrapped pipelines across the standard library, the examples and
  the tools.

- **The rehearse lens ran in a terminal of its own, which closed with the
  command.** A gate script that exits non-zero read as a failure to launch,
  and the output went before anyone could read it.

  The lens now runs in an ordinary terminal. The prompt comes back, the
  output stays, and running it again is one keystroke. The extension is
  0.3.2.

## [0.80.1] - 2026-09-18

### Fixed

- **The editor put every member of an implementation on one line.** Each
  member reported the position of the block, not of its own `let`.

  ```
  between? : ... | clamp : ... | max : ... | min : ...
  implement Ord Int =
    let max a b = if a > b then a else b;
  ```

  A member now gets a code lens above its own `let`.

- **The VS Code extension did not know `interface` or `implement`.** Both are
  declaration keywords now.

## [0.80.0] - 2026-09-18

### Added

- **Interfaces.** An interface is a contract on a module: the functions that
  the module must provide.

  ```
  -- ord.wand
  interface Ranked 'a(top: 'a -> 'a -> 'a, bottom: 'a -> 'a -> 'a)

  -- ints.wand
  let ord = import ./ord

  implement ord.Ranked Int =
    let top a b = if a > b then a else b;
    let bottom a b = if a < b then a else b

  -- main.wand
  let biggest (m: ord.Ranked Int) a b = m.top a b

  biggest ints 3 7        -- 7
  ```

  An interface takes any number of type parameters. An implementation is a
  run of `let` bindings that a `;` separates. Each binding belongs to the
  module that contains it.

  Reach an imported interface through the module that declares it, as
  `ord.Ranked`. The file that declares an interface writes the bare name.

  A member declares the effects that it performs. An effect variable that no
  argument names is an error.

- **`Ord` is a built-in interface.** The eleven ordered types implement it.
  Write it bare.

  ```
  import Int
  import Size

  let biggest : Ord 'a -> 'a -> 'a -> 'a = fn m a b -> m.max a b

  biggest Int 3 7          -- 7
  biggest Size 4KB 100MB   -- 100MB
  ```

  `Int.max`, `Int.min`, `Int.clamp` and `Int.between?` keep their types,
  their documentation and their answers. `Ord` is not a module.

### Changed

- **`wand d` marks a member that implements an interface, and aligns the
  colons.**

  ```
  $ wand d Int
  Int.abs       : Int -> Int
  Int.between?  : Int -> Int -> Int -> Bool [Ord]
  Int.clamp     : Int -> Int -> Int -> Int [Ord]
  Int.divmod    : Int -> Int -> (Int, Int)
  Int.max       : Int -> Int -> Int [Ord]
  Int.max_value : Int
  ```

  The column resets at each module. `wand d --index` and `wand d <member>`
  read the same way. `--index --json` adds `implements`, which is null for a
  member that implements nothing.

### Fixed

- **The compile cache kept a module after a module that it imports changed.**
  `wand t` reported no error for code that does not typecheck.

  ```
  dep.wand:  let n = 1        ->  let n = "x"
  mid.wand:  let d = import ./dep
             let f () = d.n + 1
  main.wand: let m = import ./mid
             m.f ()
  ```

  `wand t main.wand` reported `Int`. The fault is in 0.79.0 and all earlier
  releases.

## [0.79.0] - 2026-09-18

### Added

- **`wand t --effects`** reports what a file reaches outside itself, and
  nothing else, so a check can compare it with a policy by exit code. It
  names the file, then the set, in the notation a signature uses for one.

  ```
  $ wand t --effects examples
  examples/hello.wand                 ! {}
  examples/log-summary.wand           ! {IO}
  examples/party.wand                 ! {Shell(whoami)}
  examples/ports/disk-threshold.wand  ! {IO, Shell(df)}
  examples/ports/http-retry.wand      ! {Clock, Env, IO, Net, Proc}
  examples/sysinfo.wand               ! {Env, Shell(hostname, uname)}
  32 files
  ```

  The `!` lines up, the column set by the longest path. A file that reaches
  nothing reports `! {}` rather than a blank, so a check can match it.

  The set is the one wand inferred, never the `uses` line -- a file with no
  manifest is unbounded rather than sealed, so reading the manifest would
  report the emptiest set for the least bounded file in a tree. It names the
  labels a manifest would, so `Raise` is not among them: a raise does not
  reach outside the file. `Shell` is narrowed to the binaries the file runs,
  on the rule the manifest suggestion already follows -- only where every
  command word is literal. `Net` comes back bare, because a host list is
  declared and checked at the request rather than inferred.

  `--json` gives an array of `{file, effects}`, the narrowing split out as
  `allows` so a tool never parses `Shell(df)`. `--effects` answers one
  question, so it reports no lints and refuses `--fix`, `--strict` and
  `--expr`.

- **`wand t` takes a directory, or more than one path.** Checking a tree was
  a shell loop, because a second path was "too many arguments".

  ```
  $ wand t .
  scripts/deploy.wand: type error: 3:11: unbound variable 'targt'
  scripts/report.wand: warning: 5:1: V-BANG2: 'safe!' cannot raise, so the
    `!` promises a risk that is not there; it is 'safe'
  12 files, 1 error, 1 warning
  ```

  A directory is searched all the way down for `.wand` files, past `_build`,
  `_opam`, `.git` and `node_modules`, and without following a link to a
  directory -- the same walk `wand s` uses to find tests. Every finding
  carries its path, and one file's error does not stop the rest, because a
  gate wants the whole list rather than the first line of it. The last line
  counts what was covered, and it is printed even when nothing was found,
  since silence reads the same as finding no files to check.

  The exit code is 1 if any file has an error, or if `--strict` is given and
  any file has a violation. `--json` gives one array for the whole run, each
  object carrying its `file`. `--fix` writes every file it can fix.

  One file named on its own is unchanged: it reports what the file checks
  out as, with no path and no count.

- **The `String.to_*` family has its raising siblings.** The naming rule is
  that a fallible function comes as a pair, and this family was twelve
  exceptions to it in one module.

  ```
  $ wand -e 'String.to_url! "https://example.com"'
  https://example.com : URL
  ```

  `to_int!`, `to_float!`, `to_bool!`, `to_glob!`, `to_url!`, `to_ipv4!`,
  `to_cidr!`, `to_port!`, `to_version!`, `to_size!`, `to_datetime!` and
  `to_duration!`. Each answers with the value and raises the reason the
  plain name would have returned, so `try` gives that `Result` back.
  `to_path` has none, because it cannot fail: any text is a path.

- **`wand d --index`** prints every module's members with their signatures in
  one listing -- 547 lines, the whole standard library surface. It is what a
  shell loop over `wand d <module>` produced, as a command that stays in step
  with what is on disk.

  ```
  $ wand d --index | head -2
  Args.help? : List String -> Bool
  Args.parse : Decoder 'a -> List String -> Result String 'a
  ```

  `--json` gives the same as an array of `{name, type}`.

### Changed

- **`wand t` reports every unbound name in one run.** A failing typecheck
  answered with one error, so a file with six unknown names took six runs to
  clear -- a person reread it between each, and a tool driving `wand t` in a
  loop paid a round trip per name.

  ```
  $ wand t report.wand
  Error: type error: 1:11: unbound variable 'alpha' -- 'wand d' lists the modules...
  Error: type error: 3:11: unbound variable 'beta' -- 'wand d' lists the modules...
  Error: type error: 5:11: unbound variable 'gamma' -- 'wand d' lists the modules...
  ```

  An unbound name is the one error a check can carry on past: the name gets
  a fresh type variable, which unifies with anything, so nothing below the
  miss reports a consequence of it as a mistake of its own. Each name is
  reported once however many times it is written.

  Every other error still stops where it stood. A type that did not fit
  leaves the wrong type behind, and carrying on from one invents mistakes
  the file does not have. Where a file has both, the names are the answer.

  `--json` gives one object per name, and the language server lists them all
  in the Problems pane rather than one at a time.

- **`String.lines` drops the empty piece a trailing newline left.** A newline
  ends a line rather than separating two, so nearly every file gave back one
  element more than it had lines, with `""` at the end, and every caller had
  to filter it.

  ```
                          -- before            -- now
  String.lines "a\nb\n"    ["a", "b", ""]      ["a", "b"]
  String.lines ""          [""]                []
  ```

  Only the piece after the last newline goes, and only when it is empty, so
  a blank line written on purpose is still a line: `String.lines "a\n\nb\n"`
  is `["a", "", "b"]`. `Shell.lines` already read this way, so the two agree
  now.

- **A named field's type can be a function, written without parentheses.**
  The comma or the closing parenthesis ends a named field, so a pair around
  the whole type said nothing.

  ```
  -- before                          -- now
  type Handler(                      type Handler(
    ok: (Bool -> Int),                 ok: Bool -> Int,
    eq: ('a -> 'a -> Int)              eq: 'a -> 'a -> Int
  )                                  )
  ```

  A function type written as a parameter keeps its parentheses, because
  there they say which type it is: `raises: (Unit -> Int) -> Int` takes one
  function, and `raises: Unit -> Int -> Int` takes two arguments. A
  positional field is unchanged -- `Pair Int Int` is two fields, so nothing
  but the next field ends one. `wand f` writes the bare form, and rewrote
  the six fields in `Test.wand` that carried parentheses they did not need.

- **`wand f` no longer crawls on deeply nested code.** Two things cost it.
  It built each level's text by copying what the level below it had built,
  where it now joins the pieces and writes them out once. And it wrote a
  value out to find whether the value fits, where it now measures first and
  writes only what it keeps.

  ```
                          before    after
  400 nested if/else      189.1s    0.04s
  2000 nested lists        50.5s    0.09s
  400 nested calls          5.9s    0.17s
  400 nested matches        1.1s    0.03s
  ```

  Formatting is unchanged: the corpus, the tools, the demos and every
  regression input come back byte for byte as before.

- **A constraint in a signature now names its variable.** `Ord`, `Add` and
  `Num` printed as though they were types, so a signature could not say which
  of its uses were the same type -- and where there were two, it said nothing
  at all:

  ```
  before:  pair_of_maxes : Ord -> Ord -> Ord -> Ord -> (Ord, Ord)
  after:   pair_of_maxes : 'a: Ord -> 'a -> 'b: Ord -> 'b -> ('a, 'b)
  ```

  Six `Ord`s there, and two types: `pair_of_maxes 1 2 "a" "b"` is legal.

  `'a: Ord` is a type you can write as well as read, so what `wand d` prints
  still pastes back as an annotation:

  ```
  let bigger : 'a: Ord -> 'a -> 'a = fn a b -> if a < b then b else a
  ```

  A constraint written bare is unchanged -- it is the same constraint with no
  name to tell it from another, which is all a signature with one of them
  needs.

- **`max`, `min`, `clamp` and `between?` moved onto the ordered types.** The
  `Ord` module is gone. Each of the eleven types wand orders carries the four
  itself, so the place to look for one is the type in your hand.

  ```
  Ord.max 3 7            ->  Int.max 3 7
  Ord.min 4KB 100MB      ->  Size.min 4KB 100MB
  Ord.clamp 1s 30s 5min  ->  Duration.clamp 1s 30s 5min
  ```

  The old spelling says where its function went:

  ```
  'Ord' is not a module: the comparisons sit on each ordered type. Write
  'Int.max', 'Duration.min', 'Path.between?' etc.
  ```

  `List.max`, `List.min`, `List.sum` and the three on `Stream` are unchanged --
  a list is where you already look for those.

## [0.78.0] - 2026-09-17

### Fixed

- **`Shell(echo)` let a script run `whoami`.** wand read every `$((` as
  arithmetic and stopped looking. Where the brackets close early, `sh` reads
  it as a command and runs what is inside. This typechecked, and printed the
  user name:

  ```
  uses {Shell(echo), IO}
  IO.println $(echo $((whoami) ))
  ```

  It is refused now:

  ```
  this command runs 'whoami', which Shell(echo) does not allow
  ```

- **`Net(api.github.com)` let a script reach any other host.** A URL written
  out carried the manifest with it. One the script worked out at run time --
  from `String.to_url`, `URL.join` or a `URL.with_*` setter -- carried
  nothing, so nothing was checked. This sent the request and said nothing:

  ```
  uses {Net(api.github.com), IO}
  match String.to_url "https://anywhere.test/x" with
  | Ok u -> IO.println "%{HTTP.get u}"
  | Error e -> IO.println e
  ```

  It is refused now, at the send and at every redirect:

  ```
  this request reaches 'anywhere.test', which Net(api.github.com) does not allow
  ```

- **A `with` could not be a binding's body after a `;`.** Every other block
  form could. The `;` ended the definition instead, and the line below fell
  through to the file. This is what `wand f` writes for such a chain, and it
  did not parse:

  ```
  let f! () =
    let n = 1;
    with FS.temp_dir "t_" as d -> n
  ```

- **`Proc.exit` crashed instead of exiting**, under `wand -e`, `wand s` and
  `wand d -t`. A test file that exits on purpose reported a crash and the
  code 2. It reports its own code now, and an interrupt reports 130.

- **`--lint` and `--strict` were dropped when the mode came first.**
  `wand deploy.wand --strict` refused a violation. `wand --dry-run
  deploy.wand --strict` ran the script and passed `--strict` on to it as an
  argument. Both spellings are a gate now.

- **A script with no `.wand` extension could not be run.** `wand deploy`
  opened `deploy.wand`, so a shebang file was unreachable through its name.

- **A `DateTime` outside the years 0 to 9999 killed the run**, past any
  `try`. An instant is written with four digits for the year, so there was
  no way to write one down:

  ```
  >> DateTime.on 10000 1 1
  Error("10000 is outside the years wand writes: 0000 to 9999")
  ```

- **A lex error named one byte of a character, and was not valid UTF-8.**
  A harness reading diagnostics as text died rather than reporting, and the
  byte said nothing anyone could act on. The characters a pasted document
  carries now name the one to write instead:

  ```
  >> let x = 1 — 2
  lex error: 1:11: unexpected character '—' -- write a hyphen, -
  ```

  Curly quotes, an en dash, an ellipsis and a no-break space say the same.
  A byte that begins no character is named rather than printed.

- **A directory it could not read killed a `FS.glob_in`**, with a failure
  no `try` could catch. `try` catches it now.

- **`Env.set` corrupted the environment** rather than refusing a name that
  is not one:

  ```
  >> Env.set "A=B" "x"
  Error: Env.set: "A=B" holds '=', which separates a name from its value
  ```

  It set `A` to `B=x` before. An empty name is refused too.

- **`Duration.seconds` and its siblings wrapped round silently** on a
  number too big to hold, where `+` has always said so.

- **The YAML reader had a limit on alias expansion and none on nesting.** A
  document fifty thousand deep took seconds and could end the run:

  ```
  this document nests more than 200 deep, which is past what a value here may do
  ```

- **The editor stopped reporting anything** after one bad request. A
  position outside the file ended the language server for the session.

- **`:reload` ran the old code.** Editing a file the script imports and
  reloading said it had reloaded, and used the version from before the
  edit.

- **`Map.map` refused a constructor** where `List.map` took one:

  ```
  >> Map.map Some {a = 1}
  {a = Some(1)}
  ```

- **The VS Code extension coloured code wand rejects.** `(* ... *)` looked
  like a comment, `14:30:00` like a value, `and` and `or` like operators,
  and `for`, `do`, `end`, `class`, `instance`, `orphan` and `of` like
  keywords. Every one of those is an error when you run it. Three of the ten
  effect names -- `Net`, `Clock` and `Random` -- were left uncoloured inside
  the `uses` line that declares them.

- **The VS Code extension described hover as showing "effect rows".** The
  word is effects; the marketplace text said otherwise.

- **The VS Code extension said nothing useful when it could not start.** A
  missing `wand` showed up as "extension failed to activate". It names the
  path it tried now, and offers to open the setting that fixes it. Changing
  `wand.path` takes effect at once rather than after a window reload.

### Changed

- **Three names that ran a command without checking the manifest are gone:**
  `process_run`, `process_run_quiet` and `process_exit_code`. No wand
  program could reach them.

- **`class`, `instance`, `orphan` and `let*` are names you can use.** They
  were reserved for nothing: no part of wand read them.

- **Four ported examples were wrong.** `stage-release.wand` exited 1 every
  run and its lock was a file two runs would both write; it uses `FS.lock`.
  `dir-budget.wand` skipped names with no dot (`**.*`, now `**`).
  `rotate-backups.wand` deleted every `.tgz`, not its own `backup-*.tgz`.
  `normalize-names.wand` renamed one file onto another.

## [0.77.1] - 2026-09-16

### Fixed

- **`wand f` wrote a contract body that opened with a glob.** The brackets
  that keep such a body apart from the clause above it went straight onto
  the glob's star.

  ```
  let f n =
    requires n > 0
    **/*.wand
  ```

  The body came back as `(**)`, which the lexer reads as an attempt at a
  block comment, so the file stopped parsing. The bracket now takes the
  space the three earlier findings of this shape already had. Found by the
  daily fuzzer.

## [0.77.0] - 2026-09-15

### Added

- **A keyword names a field.** A field name is not a name any scope can see,
  so `type`, `when` and the rest read as themselves after a `.` and inside a
  constructor's brackets.

  ```
  type Condition(type: String, status: String)

  let c = Condition(type = "Ready", status = "True")
  c.type                              -- "Ready"
  Condition(c, type = "Available")
  ```

  A derived `T.decoder` reads such a document, and `JSON.of` writes the word
  back out unchanged. The short form is the one position this does not
  reach: `Condition(type, status = s)` says to write `type = type_`.

- **`Decode.and_map`** reads a record of any width as a pipeline, where
  `map2` and `map3` stopped at three.

  ```
  Decode.succeed (fn a b c -> T(a = a, b = b, c = c))
    |> Decode.and_map (Decode.field "a" Decode.int)
    |> Decode.and_map (Decode.field "b" Decode.int)
    |> Decode.and_map (Decode.field "c" Decode.int)
  ```

  A type whose field names match the document needs none of this --
  `T.decoder` is derived and reads any width. Reach for `and_map` when the
  names differ, or when the document spells a field something wand cannot,
  like `$ref`.

### Fixed

- **A type's derived decoder is reachable through its module.**
  `Apps.Deployment.decoder` read as a construction of `Deployment` and
  reported that the type was missing its fields. The workaround was to
  rename the type on import, which defeats the qualifier when several
  modules declare a `Deployment`.

  ```
  let Apps = import ./k8s/apps/v1
  let Core = import ./k8s/core/v1

  JSON.decode Apps.Deployment.decoder doc   -- reads Apps's type
  JSON.decode Core.Deployment.decoder doc   -- reads Core's
  ```

  `encoder`, `usage` and `parser` are reached the same way.

- **A derived decoder builds the type of the module that declared it.** A
  field holding another of the module's own types decoded into whichever
  module was loaded last, so two modules that each declare a `Meta` gave
  values that printed alike and were not equal. The one loaded first was
  the one that broke.

- **A type with no derived member says so under the name written.** The
  message named the type by its internal key, which carries the module's
  full path.

- **An error inside a `%{...}` points at where it was written.** The body of
  an interpolation is read on its own, and its positions started again at
  1:1 -- so a mistake in a string near the bottom of a file was reported at
  line 1, at a column that line may not have had.

  ```
  10 |  IO.println "%{Point}"

  was:  Error: type error: 1:1:   constructor 'Point' has named fields
  now:  Error: type error: 10:15: constructor 'Point' has named fields
  ```

  Every form that holds one is fixed: `"..."`, a backtick string, `$()`,
  `$?()` and `$*()`.

- **Importing a module no longer costs more the larger it is.** The cost grew
  with the square of the module, so a file that imported a large one spent
  its run deciding which constructors were in scope rather than checking
  itself. A module of 700 types took 52.7s to import and now takes 1.45s --
  the module alone typechecks in 1.23s, so importing it costs 0.22s on top
  rather than 51s. Startup is unchanged.

## [0.76.0] - 2026-09-15

### Fixed

- **A definition's body takes statements, as a binding's body already did.**
  The same two lines ran or did not, depending on whether a binding happened
  to come first.

  ```
  -- this was a type error
  let go () =
    IO.println "one"
    IO.println "two"

  -- this ran, and still does
  let go () =
    let a = 1;
    IO.println "one"
    IO.println "two"
  ```

  A binding anchors at its own `let`, so a line level with it starts a new
  statement. A definition's body kept the definition's column, so a line one
  indent in was past it and read as more of the same expression -- which is
  how two `IO.println`s became one applied to the other. The body anchors at
  its own column now, where it begins a line of its own.

  Cuddled after the `=` or an arrow it keeps the outer anchor, because there
  the body starts right of the lines below it:

  ```
  let f =
    fn p -> String.replace
      "a" "b" p
  ```

  Both spellings of a body now print the same way, which is the point of the
  fix -- `wand f` writes the block form for either:

  ```
  let go () = (IO.println "one"; IO.println "two")
  ```

## [0.75.0] - 2026-09-15

### Changed

- **`wand f` writes `;` where it wrote `in`.** `in`, a block's `;`, and the
  newline that ends a right-hand side all bind the name over the same body,
  so they are one thing to the compiler and come back one way.

  ```
  -- before
  let of_hex algorithm text =
    let want = _hex_length algorithm in
    let lower = String.lower text in
    String.length lower == want

  -- after
  let of_hex algorithm text =
    let want = _hex_length algorithm;
    let lower = String.lower text;
    String.length lower == want
  ```

  A chain of bindings needs no brackets either, so a body written as a block
  loses them:

  ```
  -- before
  let timed thunk = (
    let before = clock_elapsed ();
    let answer = thunk ();
    (clock_elapsed () - before, answer)
  )

  -- after
  let timed thunk =
    let before = clock_elapsed ();
    let answer = thunk ();
    (clock_elapsed () - before, answer)
  ```

  Two spellings survive, and each says something the `;` cannot. `in` stays
  where the value ends on a `match` or `handle` arm -- an arm runs to the
  next `|`, so a `;` on one is read as part of it, and `in` is a keyword no
  arm can swallow. `in` also stays where it narrows: written ahead of a `;`
  it keeps the name off the statements below, which is meaning rather than
  spelling.

  The brackets stay where the statements are not all bindings. A `;` outside
  them ends a binding's right-hand side and hands the rest to its body, and
  that is the whole of what it does there -- a statement that binds nothing
  is not joined to what follows by a `;` any more than by a newline.

  Nothing you have written stops working: all three spellings still parse,
  and only what `wand f` prints has changed.

### Fixed

- **A stray `in` says what it is for.** `in` belongs to a `let`, and reached
  without one the message named the bracket or the token instead:

  ```
  (fn () -> g x in y)
  -- before: expected ), got in
  -- after:  'in' closes a 'let' and there is none open here; a binding is
  --         'let name = value in body', or 'let name = value;' with the
  --         body below
  ```

## [0.74.0] - 2026-09-15

### Added

- **A binding in a function body can end with `;`.** A `;` after a binding
  ends its right-hand side and hands the rest to its body, which is what a
  newline already did. The brackets are no longer needed for this:

  ```
  let of_hex algorithm text =
    let want = _hex_length algorithm;
    let lower = String.lower text;
    String.length lower == want
  ```

  Indentation decides, as it does for a newline: at or past the binding's
  column the rest is the body, and back inside it the `;` ends the
  statement. Two statements that bind nothing still want the brackets -- a
  newline does not join those either.

  `wand f` still writes `in` here.

### Fixed

- **A type alias read a list of bare field names as a payload.** An alias
  names its target's constructor, so it builds and matches what the target
  does -- the punned form included, where a bare name is the field of that
  name.

  ```
  type Pod(name: String, tries: Int)
  type Target = Pod
  let mk name tries = Target(name, tries)
  ```

  This reported `expected String, got ('a, 'b)`. It builds a `Pod` now, and
  the pattern `Target(name, tries)` matches one. The named form always
  worked, and it is what `wand f` shortens to the punned one -- so a file
  that typechecked could come back rejected after a format.

- **`A-BIND1` missed a `let _ =` whose value is on the next line.** Only a
  value on the binder's own line was reported, and the warning landed on the
  wrong line when it did fire.

  ```
  let _ =
    IO.println "a"
  ```

  This said nothing. It is reported now, at the binder. `wand t --fix` takes
  the binder off where the value shares its line; where it does not, the
  message asks you to write the statement out.

- **A top-level `let _ =` took the rest of the file as its body.** Every
  statement below it became part of one binding, and `wand f` wrote them
  back as a single bracketed block. A named binding was never affected.

  ```
  let _ =
    FS.copy ./a ./b

  IO.println "done"
  ```

  Those are two statements again.

- **`wand f` wrote a contract body that did not parse.** A body that opens
  with an operator lost the brackets that told it apart from the clause
  above it.

  ```
  let f n =
    requires n > 0
    (-n)
  ```

  The brackets went, and `-n` under `requires n > 0` re-read as
  `requires n > 0 - n`, which leaves the contract with no body. They stay
  now.

- **`wand f` put brackets around a call that wrapped.** A line indented past
  the statement above it continues that statement, so a call that runs onto
  more lines is still one call and needs nothing to say so.

  ```
  -- before
  let seconds =
    (List.fold_left
      (fn total (_, duration, _) -> total + Result.default 0 (String.to_int duration))
      0
      calls)

  -- after
  let seconds =
    List.fold_left
      (fn total (_, duration, _) -> total + Result.default 0 (String.to_int duration))
      0
      calls
  ```

  The brackets are still written where a line does fall back to the
  statement's own column, which is where they are the difference between one
  statement and two.

- **A closing bracket took a line of its own more often than it earned
  one.** It now does so in the two places it says something: where the last
  line is a `match` or `handle` arm, and where more arguments follow the
  bracket.

  ```
  -- before
  (Shell.stream $*(printf "a\nb\n")
    |> Stream.filter (fn l -> l == "b")
    |> Stream.to_list
  ))

  -- after
  (Shell.stream $*(printf "a\nb\n")
    |> Stream.filter (fn l -> l == "b")
    |> Stream.to_list))
  ```

  An arm keeps the line, because a bracket sitting on one reads as part of
  it. So does a bracket with arguments after it, because the break is what
  puts them at the start of a line. A chain of decoders or of pipeline
  stages has neither, and loses a line per level.

### Changed

- **An import statement ends with the module name.** Anything else on the
  line is a parse error, and the message names the way to the module:

  ```
  import Config(x)
  -- error: put this on a line of its own; it binds 'Config',
  --        so 'Config.member' reaches into it
  ```

  What followed the name used to become a second statement on the same line,
  so `import A(import B)` parsed as two imports where one was written.

## [0.73.0] - 2026-09-12

### Added

- **`A-BIND1` finds a `let _ =` that binds nothing.** `let _ =` says a
  failure is being thrown away on purpose. When the value is `Unit` there is
  no failure, so the binder does nothing. `wand t --fix` takes it off.

  ```
  let _ = IO.println "done"     -- before
  IO.println "done"             -- after
  ```

  Inside a function body it cannot be taken off on its own, because the
  statements would run together. `wand t` names the line and you write the
  `;`. A `let _ =` over a `Result` is left alone -- that one is doing its
  job, and `V-DROP1` is the rule that asks for it.

### Fixed

- **`wand f` pulled apart a call that ends in a constructor.** It leaves it
  as written now.

  ```
  -- before
  let response =
    (HTTP.request!
      HTTP.Request(
        url = endpoint,
        headers = auth
      ))

  -- after
  let response =
    HTTP.request! HTTP.Request(
      url = endpoint,
      headers = auth
    )
  ```

- **`wand f` wrote `if` lines past the right margin.** A `then` branch that
  does not fit moves to its own line now, the way an `else` already did.

  ```
  -- before, 113 columns
  if HTTP.ok? response then JSON.parse body |> Result.and_then (JSON.decode d) |> Result.get!

  -- after
  if HTTP.ok? response then
    JSON.parse body |> Result.and_then (JSON.decode d) |> Result.get!
  ```

- **`wand f` dropped the brackets around an `import` used as a value.**
  `(import O) xs` came back as `import O xs`, which reads as an import and a
  separate statement rather than one call. Found by the daily fuzzer (#25).

## [0.72.0] - 2026-09-10

### Changed

- **An HTTP method is written with its module.** `HTTP.GET` and `HTTP.POST`
  rather than bare `GET` and `POST`. The six methods were built-in
  constructors, in scope in every file the way `Ok` and `Some` are, and they
  are HTTP's words rather than the language's: that list is `Ok`, `Error`,
  `Some` and `None`, and six HTTP verbs were sitting in it. A file could
  always declare its own `PATCH` over the built-in, so what this buys is not
  a name back but a bare `POST` that says where it comes from. `HTTPMethod`
  stays a built-in type, because the compiler names it -- it is a field of
  the `Net!http` payload, and a module's types are keyed by a path that
  moves with `WAND_STDLIB` -- but its constructors are reached through
  `HTTP`, in an expression and in a pattern alike. Bare `POST` names nothing
  and says to write `HTTP.POST`. One position still accepts the bare name
  and is left alone: `HTTP.Request(method = POST)` typechecks, because a
  qualified construction reads that module's names for its field values as
  well as for its own, which is how every module has always read. Write
  `HTTP.POST` there too -- it is the same value, and it is the spelling that
  reads the same wherever it appears

## [0.71.0] - 2026-09-10

### Added

- **`Stream.tally` and `List.tally`** count how often each string appears.
  Counting written as a fold applies two functions to every item -- the
  fold's own and the one `Map.update` increments with -- so the interpreter
  runs the loop as well as the counting. `tally` is the loop itself, built
  in, and enters nothing above it: counting the first field of a 200k-line log went
  from 379ms to 108ms, against 125ms for the same thing in Python. Counting
  by something other than the whole line is `Stream.map` or
  `Stream.filter_map` in front of it, and those stages run in the same loop.
  A stream that does not fit in memory still tallies -- what it holds is one
  count per different string, not the stream. Entries come back in the order
  their keys were first seen, as everywhere a map is built

### Changed

- **`Map.get`, `List.get` and `String.word` answer with the `Option`
  itself.** Each was `Result.to_option` over the primitive underneath: an
  `Ok` built, a wand function called, a match, and a `Some` built, to
  convert a value the primitive already had. Five steps a lookup, on three
  of the functions a script reaches for most, and the error string they
  built was thrown away by every caller. `Map.get` cost 1.22us against 480ns
  for `Map.get!`, which does the same search. It is 33% off a script that
  counts by a map key and 42% off one that reads a field. `Env.get` is
  unchanged: it is `try env_get_exn`, so the `Env!get` operation supplies
  the `String` a handler double stands in with. `None` carries nothing, so
  one value now serves every absence rather than one being built per miss

### Fixed

- **The docs said a script cannot do what it did not declare, and a
  subprocess can do anything.** `uses {Shell(curl)}` sends bytes to any host
  with no `Net`; `uses {Shell(cat)}` reads any file with no `FS.Read`; the
  same holds for `FS.Write`, `Env` and `Clock`. All five typecheck. The
  reference covered the hostile case -- a manifest is not a sandbox, and
  hostile code writes `Shell(sh)` where you can see it -- and not the
  ordinary one, so a reader narrowing `Net` to bound where a script sends
  bytes was wrong whenever the file also ran a command. `A subprocess is
  outside every label`, under Manifests, states it and says why it is not a
  gap waiting to be closed: wand reads the file rather than the binary, and a
  label it cannot infer is a label it cannot check. Making `Shell` imply the
  other nine would be truthful and useless -- every script that runs `git`
  would declare four more labels and the line would stop meaning anything.
  The README's claim is now about a script's own code. There is no lint rule
  for it: flagging `Shell(curl)` without `Net` implies the same for `cat`,
  `rm` and `printenv`, which is a database of what binaries do, permanently
  incomplete, and whose real cost is that a clean `wand t` would imply a
  bound that is not there

- **`wand f` wrote source that does not parse.** A `let ... in` cuddled onto
  `fn -> ` put its keyword at the end of that line, while its value and its
  `in` were laid out from the indent the lambda was handed -- so every line
  of the binding sat left of the `let` it belonged to, and a line left of the
  keyword reads as something new. Reduced, it is worse than the input
  reported: `let s = (fn -> let t = with a as d -> g (h "x") "y" in "")` came
  back as three statements with the `in` at the top level, a formatting that
  parses, runs, and does something else. A `let ... in` that wraps now takes
  the line under the lambda, so its keyword starts at the indent its own
  continuation uses

- **The effect table said "Nine"** above ten rows, two paragraphs above a
  sentence that already said ten. A cross-reference in the same file pointed
  at an anchor that does not exist

- **`docs/llm-authoring.md` had a heading that promised a list and gave
  none.** "What still goes wrong" was two sentences saying an honest list
  followed, and then the next heading. It is the section a reader checks
  first to see whether the rest is marketing. Removed rather than filled:
  what belongs under it is what `wand t` rejects most often across real
  files, and nobody has measured that

## [0.70.0] - 2026-09-09

Two more of the things the benchmark found, and a directory retired.

### Added

- **`String.word n s`** answers with the nth whitespace-separated word, and
  **`word!`** raises where there is none. `List.get! n (String.words s)`
  builds a string, a cons cell and a reversal for every field to hand back
  one of them; `word` walks to `n` and builds only that. 636ns a line
  against 410ns on a seven-field log line. It follows the rule `words`
  follows -- a run of whitespace separates once, and leading or trailing
  whitespace adds no word -- and is checked against `words` at every index
  so the two cannot drift. It is not called `field`: in `JSON.field`,
  `TOML.field`, `Decode.field` and in a type error about a record, a field
  is selected by name, and this is a position

### Fixed

- **A regex literal was compiled on every evaluation.** `Re.compile` ran
  each time the expression was reached, so a pattern written inside a loop
  was recompiled once per iteration at about 5us a time:
  `Regex.match? r/ERROR/ line` over a 200k-line file spent a second
  compiling the same pattern 200,000 times, and the way to avoid it was
  knowing to lift the literal out by hand. Written inline it now costs what
  lifting it out costs. The compiled form is kept per pattern and flags as
  written, so one pattern under two flags stays two patterns.
  `Regex.compile` is not cached: its argument can be built at run time, and
  the table would grow with the data

### Removed

- **`bench/startup.sh` and `bench/throughput.sh`.** The startup-path rule
  still asks for before-and-after numbers in the commit message; time the
  two binaries directly. `README` no longer offers the script as a way to
  repeat its startup figure

## [0.69.0] - 2026-09-09

### Added

- **`Map.update key absent f m`** changes what a key holds, where `set`
  replaces it. `f` is given what is there, or `absent` where the key is new,
  and the map is read and written in one pass. Counting was a `match` on
  `Map.get` with a branch for the first count -- four lines, and the branch
  where the seed goes wrong. The three tallies in this repository are each
  one line now. `set` is unchanged and is still the way to write a value
  that does not depend on the old one: it ignores what is there, so it
  applies no function

### Changed

- **A `;` inside a body that is not in parentheses is a parse error.** It
  used to end the definition, quietly, and turn the indented lines below it
  into top-level statements:

      let go () =
        IO.println "one";
        IO.println "two"

  printed `two` and then `one`, because the second line became a statement
  of its own and those run in file order. Nothing said so, and the
  `!`-naming lint then reported that `go` cannot raise -- true of what was
  parsed and the opposite of what was written. The error names the fix. A
  `;` separating top-level items on one line is unaffected

- **A map holds its entries in a search tree and remembers when each key was
  first added.** It was an association list, so a lookup read every key
  before it and a write walked the whole map: 200,000 counts over 400 keys
  spent two seconds in there. Everything a map promises is unchanged --
  entries come back in the order their keys were first added, a key already
  present keeps its place, `from_list` lets the last value win at the first
  appearance, and `merge` leaves the left map's keys where they are

- **A module namespace carries an index.** `String.length` was a walk of
  every member of `String`, and the list runs backwards, so the function
  declared first in a file was the last one found: 584ns to resolve, against
  74ns for the one declared last. Both are 92ns now

- **A standard library definition that only passes its argument on is the
  thing it passes to.** `let trim s = str_trim s` hands `s` over unchanged,
  so the wrapper around it existed to do nothing else. 289 of the library's
  530 definitions have that shape

### Fixed

- **The non-exhaustive-match error suggested syntax the parser rejects.** It
  printed a missing case as `_ : []`, using the cons spelling removed in
  0.31.0 and without the brackets a list pattern is written with. Copying it
  into the source gave `cons is '::'` -- one error telling you to write what
  the other refuses. It reads `[_ :: []]`

- **`Stream.unique` cost the square of what it read.** It held
  everything it had seen in a list and searched it for every item, so 20,000
  distinct lines took six seconds and 200,000 would have taken ten minutes.
  It files them the way `List.unique` already did. Its doc warned about
  memory, which was the wrong resource

- **`String.contains?` read the whole string after it had its answer.** The
  scan had no early exit, and it asked the question at each position by
  allocating a fresh substring. `String.replace` allocated the same way.
  Both compare in place now, and `contains?` stops at the first match

- **`List.sort_by` computed its key on every comparison.** It sat inside the
  comparator, so ordering n elements applied it about 2n log n times where n
  would do -- seven million calls to sort 200,000 rows. It decorates, sorts
  and drops the keys now, and `f` runs once per element, left to right

- **Reading an instant cost more than the rest of decoding one.** The
  scanner built the date with `Printf.sprintf` and tested each character
  after it by rebuilding a five-element list

- **`wand d` said `FS.glob` answers with paths.** It answers with files: a
  directory whose name fits the pattern is not in the answer. The example
  wrote a file and matched it, so it read the same either way; it creates a
  directory alongside now. `FS.list_dir` is what reads a directory's entries

- **The reference said a glob walk and the glob predicate cannot disagree.**
  They do, on exactly one case. `Glob.matches?` reads a name and performs
  nothing, so it cannot look at the disk: it answers `true` for a directory
  called `notes.txt` against `*.txt`. `FS.glob` walks and answers with
  files, so that directory is not in its answer. The two read a pattern the
  same way and answer different questions, and the reference says so now.
  The test that covered this used a tree of files alone, which cannot tell
  the two readings apart

- **`docs/reference.md` named the effect `FsRead`.** It is `FS.Read`

## [0.68.0] - 2026-09-09

A pre-release security review of the whole tree, and what it found. Six
areas were read: shell execution, the effect system, the filesystem and the
cache, the CLI and language server, CI and supply chain, and the standard
library's parsers. Twenty-six findings are fixed.

### Changed

- **An IPv4 octet may not have a leading zero.** `192.168.001.1` was an
  address and is not one; `010.8.8.8` was `10.8.8.8`, which is private,
  while libc and every resolver read `010` as octal and make it `8.8.8.8`,
  which is not. A script that asked `IPv4.private?` and handed the same text
  to a command checked one host and reached another. A lex error names the
  rule, in a literal and in `IPv4.of_string`, `String.to_ipv4` and
  `CIDR.of_string`
- **A `JSON` value cannot hold an infinite or NaN number.** `JSON.parse
  "1e999999"` used to answer `Ok` with a value that could not be written
  back out. `parse`, `read_file`, `of` and `of_float` refuse at the door,
  and the `!` forms raise. `YAML` reads into the same value and answers the
  same way, so `.inf` is an error there
- **`wand s` does not walk into a symlinked directory.** A run covers what
  the tree holds. A directory named on the command line is still searched,
  whatever it is, and a linked *file* is still read
- **A regex may repeat at most 10,000 times.** `a{999999999}` never
  finished: a counted repeat is expanded where a pattern becomes a `Regex`,
  so the count alone decides the cost. Matching is unaffected -- it is a
  non-backtracking automaton
- **`Env.clear` takes the name out of the environment** instead of setting
  it to the empty string. A variable holding nothing is still set, so
  `${NAME-fallback}` in a child took the empty string and never reached the
  fallback
- **`wand f` and `wand t --fix` write the file the way `FS.write_atomic`
  does.** Beside and renamed into place, mode kept, symlink written through.
  They truncated the source and then filled it, so a crash part way through
  left half a file
- **A release archive built by CI carries a build attestation.** Check one
  with `gh attestation verify <archive> --repo wand-lang/wand`. The `.sha256`
  beside an archive is written by the job that builds it, so it
  authenticates the download and not the publisher

### Fixed

- **`wand t` ran the code it imported.** Typechecking evaluated every
  imported module under the handler a real run uses, so `wand t victim.wand`
  on a file importing a hostile one created a file and exited 0. The
  language server typechecks on every keystroke, so opening a file in an
  editor ran what it imported. Analysis paths load a module for its types
  and never evaluate its body; running a script, running a test file and the
  REPL still do
- **A newline in a command bypassed the `Shell(...)` allowlist.** The shell
  reads one as a separator and wand read it as whitespace, so the word after
  it was never a command position. Under `uses {Shell(echo)}`, `$(echo
  %!{c})` with a newline in `c` ran whatever followed and exited 0
- **Two crashes a script could not catch.** A JSON number that read as
  infinity, and a NUL byte in a shell splice, each ended the program with a
  fatal error past `try` and past `$?()`. Both are ordinary raises now
- **`FS.lock` leaked into spawned children.** The lock file was the one
  descriptor opened without `O_CLOEXEC`, and an flock belongs to the open
  file description -- so a script that started a background process while
  holding the lock left the next run reporting `Held` with nothing holding
  it, which is the cron guard the lock exists for
- **`Size` and `Duration` addition wrapped.** `4000000000GB + 4000000000GB`
  answered with a negative `Size`, which passes any threshold a script
  compares it against. Both use the checked add `Int` addition uses
- **A corrupt compile-cache entry segfaulted every later run.** Marshal
  reads what it is given, so a header it accepted and a body it walked into
  was a fault, which nothing can catch -- the entry stayed and the script
  died the same way until the cache was deleted by hand. An entry carries a
  digest of its bytes, checked before anything is unmarshalled, and the
  write reaches the disk before the rename
- **One malformed frame killed the language server.** A negative
  `Content-Length`, a huge one, or a body that was not JSON each ended it,
  and every later request in that session went unanswered. A bad body is
  answered with `-32700` and the session goes on
- **The Rehearse lens ran the file name.** It built a shell command with
  `JSON.stringify` around the path, and double quotes do not stop a shell
  reading `$(...)`, so a file named `x$(cmd).wand` ran `cmd` when the lens
  was clicked. The terminal spawns from an argv
- **`FS.delete` on a symlink to a directory** answered "Not a directory". It
  removes the name it is given
- **`FS.copy_tree` could not be re-run** over a tree holding a link that
  points nowhere
- **`FS.copy` read the whole file into memory.** A block at a time: a 1 GB
  copy peaks at 7.0 MB of resident memory instead of 1.08 GB
- **`FS.temp_dir` had a window** between removing a unique name and making a
  directory of it. `mkdtemp` has none
- **`install.sh` could not resolve "latest" without curl.** The wget
  fallback never asked wget to print the headers it was scraping, and
  matched them with a pattern that ignored wget's indent. It is gone rather
  than repaired: no CI runner lacks curl, so the branch never ran anywhere,
  and the text it read differs between GNU wget and the busybox one on the
  machines most likely to lack curl. Without curl the script says so, and
  names where to get the archive instead
- **`FS.delete_tree` named every step by path.** `lstat` said a name was a
  directory, `readdir` listed it, `rmdir` removed it -- three lookups of one
  name, so something able to write inside the tree could replace a directory
  with a symlink between two of them and send the deletions elsewhere. It
  walks by directory descriptor now, which is what `rm -rf` does. Nothing
  observable changes; the cost is one descriptor per level of depth

### Editor

- **`wand.autoEdit`** turns off the import and manifest edits the server
  pushes as you type. Default on, and the same fixes stay available as quick
  fixes
- **`wand.path` is machine scope**, so a workspace cannot point the language
  server at a script in the repository

### CI

- The `wand-lang/setup-wand` checkout is pinned to a commit. It runs wand code
  and wand code runs shell, so a push to that repository was a command
  running in this one
- `ci.yml` declares `permissions: contents: read`; `release.yml` declares
  them per job, so the build job no longer has `contents: write`
- Every action is pinned to a commit, and `.github/dependabot.yml` bumps
  them weekly
- Trigger values reach `run:` blocks through `env:`, and the fuzz step
  checks its numbers are numbers before arithmetic evaluates them

## [0.67.0] - 2026-09-08

### Added

- **`YAML`, read-only.** Twenty members: `parse`, `parse_all`, `read_file`,
  `read_file_all`, each with a `!` sibling, plus `decode`, `field`,
  `field!`, `get_string`, `get_int`, `get_float`, `get_bool`,
  `get_sequence`, `get_mapping`, `mapping?`, `sequence?` and `null?`
- **Scalars follow the YAML 1.2 core schema, not 1.1.** Each of these is a
  real file that 1.1 reads as something other than what it says:

| written | 1.1 | wand |
|---|---|---|
| `on: push` | the key is the boolean `true` | the key is `"on"` |
| `- 2200:22` | base sixty, `132022` | the string `"2200:22"` |
| `restart: no` | the boolean `false` | the string `"no"` |
| `country: NO` | the boolean `false` | the string `"NO"` |

- Only `true` and `false` are booleans. `yes`, `no`, `on` and `off` are
  words. Quoting always makes a string
- `1.10` is a float under both schemas, so a chart version read unquoted
  arrives as `1.1`. Read it from a quoted string
- **`parse` reads a file holding one document and fails on a file holding
  more**, naming how many it found. Taking the first silently is how a
  script checks one third of a manifest and reports that everything passed.
  `parse_all` reads them all, which is what a Kubernetes manifest wants
- Anchors are per document: `---` starts a new naming scope
- **Anchors, aliases and merge keys work**, because a block merged into
  several services is how a compose file avoids repetition. An explicit key
  beats a merged one wherever it is written, and `<<: [*a, *b]` takes the
  earlier one. Expansion is capped at 100,000 nodes
- **An unknown tag is refused, by name.** `!Ref` in a CloudFormation
  template would otherwise parse, decode, and mean something entirely
  different from what the file says. The standard `!!str`, `!!int`,
  `!!float`, `!!bool` and `!!null` are honoured
- `decode` takes the same `Decoder 'a` as `JSON.decode` and `TOML.decode`,
  including the ones derived from a type definition
- **`examples/ports/manifest-limits.wand`** reports the containers in a
  manifest that set no resource limits

### Changed

- The linux-x86_64 and linux-aarch64 release builds link libyaml, which the
  `yaml` package vendors. The binary is still statically linked, and is
  about 12% larger
- `docs/http-design.md` and `docs/yaml-design.md` are deleted, their work
  having shipped. `docs/roadmap.md` goes with them: both items on it are
  done

## [0.66.1] - 2026-09-08

### Fixed

- **The linux-x86_64 binary aborted at startup on some machines.** It
  printed `Fatal error: Failed to allocate signal stack for domain 0` and
  exited 134. `install.sh` reported `the downloaded binary did not run`
- The machines are the ones whose CPU has AMX -- `Intel(R) Xeon(R) 6973P-C`
  and `INTEL(R) XEON(R) PLATINUM 8573C` among them. About one GitHub runner
  in eight is such a machine. It failed every start on those, and no start
  on any other, so a re-run only ever drew a different machine
- An OCaml runtime before 5.5.1 sizes its alternate signal stack from the
  build-time `SIGSTKSZ`, which musl fixes at 8192. A CPU with AMX makes the
  kernel ask for 11952, and musl 1.2.6 compares the request against the
  kernel's number and answers `ENOMEM`
- The release now builds with OCaml 5.5.1, which reads
  `sysconf(_SC_SIGSTKSZ)` instead. It asks for 19120 on those machines and
  starts. The binary is still statically linked
- Only the linux-x86_64 build was affected. linux-aarch64 and both macOS
  builds ask the same 8192 and are not refused, because no comparable
  register state is saved there
- **`install.sh` keeps the failing binary's stderr** and prints it above the
  failure. It ran the binary with `2>/dev/null`, so the message the runtime
  printed before aborting went unread for two releases

No language changes. Every wand program behaves as it did in 0.66.0.

## [0.66.0] - 2026-09-08

### Added

- **Seven functions in `Result`:** `reason`, `map`, `and_then`, `map_error`,
  `flatten`, `default` and `get!`. The module had `to_option`, `ok?` and
  `error?`
- `Result.map` applies a function to the value. A failure passes through
  unchanged
- `Result.and_then` applies a function that returns a `Result` of its own.
  The answer stays one `Result` deep, and stops at the first failure

```
let read path = JSON.read_file path |> Result.and_then (JSON.decode Release.decoder)
```

- `Result.map_error` applies a function to the reason. A value passes
  through unchanged. The error type can change
- `Result.reason` returns the reason as an `Option`. `to_option` returns the
  value; there was no function that returned the reason
- `Result.flatten` turns `Result 'e (Result 'e 'a)` into `Result 'e 'a`.
  `Par.map` returns one `Result` per item, so work that returns a `Result`
  comes back with two
- `Result.default` returns the value, or a given fallback if it failed
- `Result.get!` returns the value, and raises the reason if it failed. It
  takes `Result String 'a`, because the reason becomes the message. A script
  uses it to write the `!` half of its own pair. Before this, a script's `!`
  function could only raise a message from the standard library

```
let port_of s = if s == "" then Error "no port given" else String.to_int s
let port_of! s = Result.get! (port_of s)
```

### Changed

- `Par.timeout` uses `Result.flatten`. `Args.parse_with` and `Args.read` use
  `Result.and_then`. `Args._object_of_spec` and `Digest.of_hex` use
  `Result.map`
- `Digest.of_hex!`, `Base64.decode!` and `Base64.decode_url!` use
  `Result.get!`. These were the three functions that unwrapped a `Result` by
  hand to raise its reason
- Six examples use the new functions: `verify-archives.wand` (`and_then`,
  `map`, `flatten`), `pod-restarts.wand`, `http-retry.wand` and
  `release-check.wand` (`and_then`), `error-rate.wand` (`to_option`) and
  `repo-status.wand` (`default`)
- The tree had fourteen `| Error why -> Error why` arms. Only the three
  that implement `Result.map`, `Result.and_then` and `Result.flatten` are
  left
- `test/wand/test_json.wand` uses `Result.and_then`

## [0.65.0] - 2026-09-07

### Added

- **`Net`, a tenth effect label.** A file that sends bytes to a host outside
  this machine declares it. The manifest narrows it by host, and bare `Net`
  admits any:

```
uses {Net(api.github.com, hooks.slack.com)}
```

- The host is checked **as written**. wand resolves no DNS, so
  `Net(example.com)` does not stop a connection to an address the script
  writes out, any more than `Shell(git)` peels a wrapper
- **A manifest word may be a pattern.** `*` stands for part of a name, and
  stops where the name's parts divide: at a `/` in a binary, at a `.` in a
  host

```
uses {Shell(docker-*)}      -- docker-compose, but not docker
uses {Shell(./scripts/*)}   -- ./scripts/probe.sh, but not ./scripts/a/b.sh
uses {Net(*.example.com)}   -- api.example.com, but neither example.com
                            -- nor a.b.example.com
```

- A binary named without a path still matches wherever it is found, which is
  how `Shell(git)` has always admitted `/usr/bin/git`. So `Shell(docker-*)`
  admits a `docker-compose` anywhere on `PATH`. A host has no such rule: it
  is matched as written

- `Shell(*)` and `Net(*)` are errors. A pattern that admits everything is
  the bare label written at greater length, and the message says to write
  that instead

- **`HTTP`.** `request`, `get`, `post`, `download`, `upload`, each with a `!`
  sibling, plus `ok?`, `header`, `header_list` and `decode`. `HTTPRequest`,
  `HTTPResponse` and `HTTPMethod` are built in, so they need no import
- **A 404 is not an `Error`.** The exchange succeeded and the server said
  no. `Error` is a transport failure. `HTTP.get` answers with a response
  whatever the status; `HTTP.get!` raises on a non-2xx
- Every field of a request but the URL has a default: `GET`, no headers, no
  body, `30s`, and five redirects. **Every redirect hop is checked against
  the manifest**, so a manifest naming two hosts admits a redirect between
  them and one naming a single host does not
- `HTTP.download` writes to a file without the body becoming a value
- **`Test.with_http` and `Test.http_calls`.** The first answers requests
  from a table, the second reports the URLs a body would ask for. Both cover
  every `Net` operation, so a sealed test reaches nothing
- **`V-NET1`**, for a request built with a host decided at run time under a
  narrowed `Net`
- **An error position names its file** when the file is not the one being
  run — an imported module by path, the standard library as
  `<stdlib>/HTTP.wand`. A position in the file you asked to run stays bare
- **`HTTP.Request`, `HTTP.Response` and `HTTP.Method`** are aliases of the
  built-in three, so a file that imports `HTTP` can write the short name. The
  two spellings are one type: a value built one way annotates, matches and
  passes the other. Signatures print the built-in name
- **`Args.Parser`** is an alias of `CommandLine`, under the name the thing
  already goes by — `Opts.parser` makes one and `Args.read` takes one
- **`examples/ports/http-retry.wand`** now uses `HTTP`

### Changed

- **`--dry-run` runs `GET` and `HEAD` and withholds every other method**,
  reporting `would post: <url>` and answering `202` with no body
- **`--trace` reports a request and a download**
- **The transport runs `curl`.** wand has no TLS of its own, so a narrowed
  `Shell` does not bound it: `Shell(git)` means only `git` runs *from this
  script*. The reference and the README say so
- **`A-USES1` leaves a manifest pattern alone** rather than reporting it
  unused
- **The block-comment hint fires on fewer spellings.** An open paren
  followed by a star reported "a comment is `-- ...`", which a manifest
  pattern needs. It now fires when the star is followed by whitespace, end
  of input, or a second star

### Fixed

- **`type X = ShellResult` broke `ShellResult`.** An alias to a built-in
  record was kept as a variant declaring a nullary constructor over the real
  one, which took its fields with it for the rest of the file — whether or
  not the alias was used. An alias to a built-in record now builds, matches
  and carries its field defaults
- **A built-in constructor that carries nothing can be named.** `GET` and
  the other methods answered "unknown constructor"
- **A module's own alias resolves inside the module that declared it.** The
  alias table was keyed by the canonical name a module's types travel under,
  while the file writes the short one, so `type Response = HTTPResponse`
  worked everywhere except where it was written
- **An alias reached through its module matches as well as annotates.**
  `M.Response(...)` in a pattern covered nothing, and the run refused it
- **An alias forwards the constructor of a parameterised target**, is
  accepted applied under its module's name, and can be updated through —
  `T(base, f = x)` reported an unknown constructor
- **A built-in type cannot be declared over by an alias.** `type Int =
  String` was taken, and every `Int` after it meant `String`. Two aliases of
  one name are an error too

## [0.64.0] - 2026-09-07

### Added

- **`FS.write_atomic` and `FS.write_atomic!`.** Write a file so no reader
  sees it half-written. The content goes to a temporary file beside the
  target, and a rename replaces the target. An existing target keeps its
  mode, a symlink is written through, and the temporary file is synced
  before the rename. The containing directory is not synced. Declares
  `FS.Read` as well as `FS.Write`

```
FS.write_atomic! /etc/app/config.toml (TOML.stringify (TOML.of! settings))
```

- **`FS.write_lines_atomic` and `FS.write_lines_atomic!`.** The same for a
  stream, one element per line. Only a stream that finishes is published: a
  source that fails or a stage that raises leaves the target unchanged
- **`FS.lock` and `FS.lock!`.** An operating-system lock on a file, held for
  as long as the `with` holds it and released when the process dies. The
  acquire does not wait. It guards other `Par` workers as well as other
  processes, and is not re-entrant. The lock file is created if missing and
  is never deleted. Not dependable on NFS

```
with FS.lock /var/run/deploy.lock as taken ->
  match taken with
  | Ok _ -> deploy ()
  | Error FS.Held -> IO.println "a deploy is already running"
  | Error FS.Denied why -> IO.println_err "cannot lock: %{why}"
```

- **`FS.LockError`**, `Held | Denied String`. `Held` is another holder;
  `Denied` is a lock that could not be opened. `FS.lock!` raises on either
- **`FS.lock_wait` and `FS.lock_wait!`.** Take a lock, waiting up to a
  `Duration` for it. A budget that runs out answers `Held`; a `Denied` ends
  the wait at once, as does a wait on a lock the same bracket holds. Carries
  `Clock`
- **`Test.with_lock`, `Test.with_lock_held` and `Test.lock_calls`.** Answer
  every acquire as taken, answer every acquire as held, and report the paths
  a body locks. All three answer at once
- **`examples/ports/publish-config.wand`**, a cron-guarded publish

### Changed

- **`Test.without_writes` and `Test.writes` cover every write of a file's
  contents**, not `write_file` alone: `write_atomic`, `append`,
  `create_file`, `write_lines`, `append_lines` and `write_lines_atomic` as
  well. `delete`, `mkdir`, `rename`, `copy` and taking a lock still reach
  the disk
- **`--dry-run` reports the two atomic writes and withholds them.** It takes
  a lock rather than withholding it, and does not wait for one
- **`--trace` reports taking and releasing a lock**

### Fixed

- **V-BANG2 no longer reports a `!` on a function that answers a
  `Resource`.** `FS.lock!` raises when the bracket opens rather than when
  the name is called. V-BANG1 is unchanged, so `FS.temp_file` keeps its name

## [0.63.0] - 2026-09-06

### Changed

- **`Path` is ordered, and two spellings of one file are equal.** `/a/b ==
  /a//b` was `false` and is now `true`. Repeated separators, `.` segments and
  a trailing separator do not change which file a path names, so a comparison
  reads the path rather than the text. `./b` and `b` are one path for the
  same reason. Any script that compares a path it built against one it read
  back -- from `FS.list_dir`, from a glob, from a command -- could be wrong
  about this before
- **`<`, `>`, `<=` and `>=` take a `Path`.** The ordered set is eleven types
  now, and `Ord.max`, `Ord.min`, `Ord.clamp` and `Ord.between?` serve it
  without a new definition. `List.sort` already sorted paths and now sorts
  them by the same form the operators use
- **Three things a comparison will not do.** It does not resolve `..`,
  because `/a/c/../b` is `/a/b` only while `c` is not a symlink, and saying
  two paths are equal when they may be different files is worse than saying
  nothing. It does not fold case, because whether `/A/b` and `/a/b` are one
  file is a property of the filesystem. It does not make a relative path
  absolute, because that needs the working directory, and a comparison must
  not perform an effect to answer. So `/a/c/../b`, `/A/b` and `./b` are each
  unequal to `/a/b`. `Path.normalize` still resolves `..`, and still says in
  its own doc that it is text only
- **A path keeps the spelling it was written with.** `Path.to_string /a//b`
  is `"/a//b"`. The comparison computes the form; nothing rewrites a literal,
  which is how `DateTime` already works

## [0.62.1] - 2026-09-06

### Added

- **`Stream` gains the stages between the source and the fold.**
  `filter_map`, `flat_map`, `take_while`, `drop`, `drop_while`, `indexed`,
  `scan`, `chunks` and `unique`. `filter_map` maps and filters in one step,
  so a line that does not parse answers `None` and never reaches the fold.
  `flat_map` is the one stage that emits more than it is given, which a log
  line holding several records needs. `scan` is a running `fold_left`: each
  element answers with the total after it. `chunks` groups elements into
  lists of n, and the last group holds what is left
- **`FS.write_lines` and `FS.append_lines`.** `FS.stream_lines` read and
  nothing wrote, so a filtered log was one `FS.append!` per line, which
  opens and closes the file each time. These open the file once and write
  each element on its own line. Both have `!` siblings

```
FS.stream_lines /var/log/app.log
|> Stream.filter (fn l -> String.contains? "ERROR" l)
|> FS.write_lines! ./errors.log
```

- **`FS.stream_lines_all` reads several files as one stream.**
  `FS.glob *.log |> FS.stream_lines_all` is the case it exists for. It opens
  each file when the one before it runs out, so it holds one file open
  however many are named

### Fixed

- **`--dry-run` answers a read from what it withheld.** A rehearsal withholds
  changes and runs reads. So a script that wrote a file and read it back
  failed on the read, after reporting two steps as though they had happened.
  The rehearsal now remembers what it withheld: writes, appends, deletes,
  renames, copies, directories, the stream sinks and `Env.set`, with every
  read consulting them. Nothing is written to disk. A withheld command still
  changes nothing a read can see, because wand cannot model what a
  subprocess would have done
- **A rehearsal reports releasing a temp directory.** The release asks
  `exists?` first. In a rehearsal that was false, so the plan ended without
  the `would delete recursively` line that a real run performs

### Changed

- **A stream stage can emit more than one element.** The driver carried at
  most one value per item and now carries a list. The cost is under this
  machine's noise on a fold over 200,000 lines

## [0.62.0] - 2026-09-06

### Added

- **A command is a value.** `$*(git status)` makes a command. It does not
  run it. `Shell.run!` runs one and gives back its output. `Shell.run` gives
  back a `Result` instead of raising. `Shell.query` gives back a
  `ShellResult`. You can name a command, pass it to a function, and put it
  in a list. `$()` cannot do any of that. It runs the command and reads all
  of the output before a function sees a value

```
let backup = $*(pg_dump -Fc %{db})     -- nothing has run
List.map Shell.query [$*(git fsck), $*(git gc --dry-run)]
```

- **`Shell.stream` reads a command line by line.** `$()` reads to the end of
  the output before it answers. So `$(tail -f app.log)` never returns.
  `kubectl logs -f` and `journalctl -f` behave the same way. `Shell.timeout`
  was the only bound on this. It kills the command and gives back an
  `Error`, and you get none of the lines. A stream pulls one line at a time.
  It holds one line in memory. `Stream.take` stops the read

```
Shell.stream $*(tail -f /var/log/app.log)
|> Stream.filter (fn l -> String.contains? "ERROR" l)
|> Stream.take 10
|> Stream.to_list
```

- **A stopped read stops the command.** wand sends SIGTERM. It sends SIGKILL
  five seconds later. `Shell.timeout` uses the same grace period. wand
  signals the command it started. It does not signal that command's own
  children, so `sh -c "tail -f x"` leaves the `tail` running. A stream that
  reads to the end raises on a non-zero exit, as `$()` does. A stream that
  stops early ignores the exit code, because wand ended the command
- **Two new handler cases: `Shell!command` and `Shell!stream`.**
  `Shell!command` fires when a command is made. It carries the command line,
  and it resumes with the command line to use. A test can read every command
  a body names, including a command the body never runs. A test can also put
  a different command in its place

### Changed

- **`$()` and `$?()` are short forms.** `$(cmd)` is `Shell.run! $*(cmd)`.
  `$?(cmd)` is `Shell.query $*(cmd)`. No script's text changes. Two other
  things do. A handler that takes `Shell` out of a signature now needs a
  `Shell!command` case and a `Shell!stream` case, as well as the other four.
  And each command form performs two operations, where it performed one
- **A command performs `Shell` when it is made.** A function that only
  builds commands carries the label. `Shell` now means "runs a command, or
  names one". This keeps two checks on one site. `wand t` reads the command
  words where they are written, so a file that names `git` and never runs it
  still declares `Shell(git)`. The bound travels with the value. A command
  built in a `Shell(echo)` file is checked against that list wherever it is
  run
- **No `String` becomes a `Command`.** `Shell.run! "echo %{name}"` is a type
  error. Inside a command, `%{...}` quotes the value as one argument. Inside
  a string, `%{...}` puts the text in. The two forms look almost the same on
  the page, and only the command form is safe. The quoting belongs to the
  syntax. So a command can only be built where its words are written

## [0.61.0] - 2026-09-05

### Added

- **`wand d` says what a function performs.** A signature carries the effect
  *label* -- `! {FS.Read}` -- and a label covers a dozen operations, so the
  type alone never said what a handler case should name or what a test
  should mock. That answer was only in the reference's table, a document
  away from the question. It is now a line under the signature, and it is
  also where the naming rule stops holding: `JSON.read_file` performs
  `FS!read_file`, not `JSON!read_file`

```
JSON.read_file : Path -> Result String JSON ! {FS.Read}
performs FS!read_file
```

- **`Hash`, `Digest` and `Base64`.** A checksum written by `Shell(shasum)`
  tells a reviewer that a binary ran and nothing about what was verified,
  and it does not run everywhere -- the tool is `shasum` on macOS and
  `sha256sum` on most Linux images. `Hash.string`, `Hash.file`, `Hash.hmac`
  and `Hash.equal?` over `Sha256`, `Sha512`, `Sha1` and `Md5`. The weak two
  are here for interop and named as such: leaving them out would not stop
  anyone using them, it would send them to `Shell(md5sum)`. Every value in
  the tests and the doc examples is a published vector -- NIST for the
  digests, RFC 2202 for HMAC, RFC 4648 for base64 -- so the suite is a check
  rather than a snapshot
- **Hashing performs nothing, and the reference says why.** `Hash.string`,
  `Hash.hmac` and every `Base64` function reach nothing outside the program
  and answer the same twice, so they earn no label and a script that uses
  them declares nothing. Only `Hash.file` reaches outside, and what it
  carries is `FS.Read` for the read rather than anything for the
  arithmetic. It goes through a new `FS!hash_file` operation, so
  `--dry-run`, `--trace` and a test's mock see it the way they see
  `FS!read_file`, and it reads the file in blocks rather than turning a
  release archive into a `String`
- **A digest is a type, not a hex `String`.** `Digest` carries the
  algorithm that made it, so comparing a sha256 against a sha512 is a
  question the checker answers rather than a `false` that reads like a
  failed verification, and one value renders two ways -- `Digest.hex` for a
  checksum file, `Digest.base64` for an S3 ETag. `Hash.equal?` is
  constant-time and `==` is not; the doc says which case is which rather
  than carving `==` out for one built-in type
- **`Base64` has both alphabets**, because both are load-bearing: RFC 4648
  section 4 for a basic-auth header and a kubeconfig secret, section 5 for a
  JWT and a query parameter. `encode` pads and `decode` accepts input either
  way, which is the only pairing that round-trips against the rest of the
  world -- most JWT producers omit the padding the standard requires
- **`Ord`, a module for the four functions people write out of `<` and
  `>`.** `Ord.max`, `Ord.min`, `Ord.clamp` and `Ord.between?`. One
  definition serves all ten ordered types, so `Ord.max 30s 2min` and
  `Ord.max 1.9.0 1.10.0` are the same function rather than nine copies of
  it. The module takes the name of the constraint that appears in the
  signature, as `Option` and `Result` do, so a reader who sees `Ord -> Ord
  -> Ord` and reaches for `Ord.max` finds it where they looked. `clamp` and
  `between?` take the low bound, then the high, then the value, so the value
  can arrive from a pipeline; both bounds are included
- **`Int`, ending an asymmetry with `Float`.** `Int.abs`, `Int.pow`,
  `Int.divmod`, `Int.max_value` and `Int.min_value`. A script wanting the
  absolute value of a `Float` had `Float.abs`, and one wanting it for an
  `Int` wrote the conditional. `pow` with a negative exponent divides, and
  `Int` division truncates toward zero, so `Int.pow 2 (-1)` is `0`. There is
  no `Int.to_string` and no `Int.of_string`: wand puts a conversion on the
  type it produces, so those stay `String.of_int` and `String.to_int`
- **`List.sum`, `List.max` and `List.min`.** Each starts from the first
  element rather than from a literal zero, which is what keeps them
  polymorphic -- a `0` would be an `Int` and would pin the list to `Int`, and
  `Add` covers `Size` and `Duration` too. So `List.sum [1KB, 500B]` is
  `Some(1500B)`. The `Option` is the price of that and it is honest: a sum
  of nothing has no unit to answer with, and the unit is the very thing the
  type does not fix. Supply one where it is known, with `Option.default 0B`
- **Nine `Stream` terminal operations**: `count`, `last`, `empty?`, `any?`,
  `all?`, `find`, `sum`, `max` and `min`. `List` had thirty-one functions
  and `Stream` had seven, on the path the big files use. `empty?`, `any?`,
  `all?` and `find` stop reading as soon as the answer is settled, so a
  match near the head of a 10GB log costs the head of that log. It is
  `count` rather than `length`, because `List.length` sounds free and this
  is not: counting reads the whole source, and counting twice reads it
  twice. The different word is the warning. Every other name that `List`
  also has keeps the `List` spelling, because the answer really is the same
- **A `String` is bytes**, a section of the reference that says so. There is
  no encoding layer, and that is the right model for a language that reads
  log files, shell output, filenames and configuration: a `String` that
  validated UTF-8 would refuse to open a file with one badly-encoded line in
  it. The section names what is safe on any input -- comparison, `split`,
  `lines`, `words`, `join`, `contains?`, `trim`, `repeat`, and `replace`
  with an ASCII needle, all byte-safe by construction, because a UTF-8
  continuation byte can never be mistaken for an ASCII character -- and what
  is not: `length` and `bytes` count bytes, `upper` and `lower` convert
  ASCII only, and `slice` and `reverse` can cut a multi-byte character in
  half. `Regex` matches bytes on the same terms, so `.` is one byte and
  `r/^.$/` does not match `"é"`
- **Why `String.length` keeps its name**, in the same section. Renaming it
  would fix nothing: the question people reach for it with is a width or an
  alignment check, and a character count does not answer that either -- a
  CJK character occupies two terminal columns and a combining mark occupies
  none. Display width is a third quantity, and counting characters would
  swap one wrong answer for a different wrong answer while implying the
  question had been settled
- **`String.truncate`**, the one function in `String` that knows about
  UTF-8. `String.slice 0 4 "café"` leaves half of the é behind, and cutting
  text down for display is the case where that matters -- a commit message
  in a summary, a log line in a report. `truncate` takes at most n bytes and
  never ends inside a character, so `String.truncate 4 "café"` is `"caf"`.
  The budget is bytes, as every other `Int` in the module is, so
  `String.length (String.truncate n s)` is never above n. It is not a
  display width, for the same reason `length` is not. Bytes that are not
  UTF-8 are cut at n, because there is no character there to keep whole

### Changed

- **Seven operations are renamed to match the functions that perform
  them.** The rule is that an operation is the function you call with a `!`
  where its dot would be, so there is nothing extra to remember; these
  seven had drifted off it. `FS!exists`, `FS!file` and `FS!dir` dropped the
  `?` their functions carry and are now `FS!exists?`, `FS!file?` and
  `FS!dir?`. `Env!parse_dotenv` and `Clock!elapsed` were named for what the
  code did rather than for `Env.read!` and `Clock.timed`, and are now
  `Env!read` and `Clock!timed`. `Random!below` is `Random!int`, after the
  function it is the draw behind. `FS!hash_file` is `Hash!file`, which
  makes `Hash` the one family that is not an effect's own name -- the
  family says where the function lives, and the effect it carries is in the
  signature
- **The release pipeline checksums with wand instead of `shasum`.**
  `tools/checksum.wand` writes the `.sha256` beside a release archive, and
  the `Makefile` and `release.yml` both call it with the binary they just
  built. The output is `shasum -a 256` format byte for byte, so `shasum -c`
  next to the download still reads it. `install.sh` keeps its own shell
  fallback and has to: it runs before there is a wand to run
- **`String.chars` is now `String.bytes`.** It returned one string per
  *byte* and called them characters, so `String.chars "café"` came back with
  five elements and the last two were the halves of the é. The name was the
  defect: a caller reading it had no reason to look. Nothing about the
  behaviour changes.
  Three occurrences in this repository, and this is a breaking change the
  moment anyone outside it writes a script, which is why it lands now rather
  than after a first release
- **Two shipped examples stop folding a total by hand.**
  `examples/ports/dir-budget.wand` summed file sizes with `List.fold_left
  (fn total size -> total + size) 0B` and `examples/ports/pod-restarts.wand`
  summed restart counts the same way. Both read `List.sum |> Option.default`
  now
- **The reference no longer enumerates what the REPL preloads.** It said
  "they load every stdlib module for you" and then named twenty-two of
  thirty-five, a list that had gone stale silently. The preload is exactly
  the modules under Current standard library, and it now says so, so a
  module added there is covered the day it is added

### Fixed

- **A constructor that takes no payload hands the bracket back to the
  call.** `Hash.string Sha256 (body ++ "\n")` was an error: a bracket after
  a constructor is its payload, and the parser attaches one without reading
  how many fields it has, so that `Ctor (a, b)` means the same thing in
  every file. A nullary
  constructor cannot own it, so it belongs to the call around it, and both
  `wand t` and a run now read it that way. Nothing that compiled
  before changes meaning, because a nullary constructor is not a function
  and applying it was always an error. Where there is no call to hand the
  argument to -- `let r = Red (1)` -- it is still an error, and the message
  is the one thing there is to say: write `Red`, with nothing after it. The
  correction that bracketed the constructor is gone with it, because
  `(Red) (1)` applies it just the same
- **`wand t --fix` offered a correction that did not parse.** The one above
  replaced the constructor's own name, so on the qualified spelling it would
  have written `Digest.(Sha256)`. `--fix` checks that what it writes parses
  and refused, so nothing was ever corrupted -- but the correction was dead
  for every qualified constructor, which is the spelling a stdlib type gets
- **`stdlib/Hash.wand` and `test/wand/test_hash.wand` had no manifest**, and
  both perform. The lint suite says so; it was reported as passing off a
  truncated run
- **A session lost the types an import brought in.** A REPL or `-e` step
  that named a constructor of an imported module -- `Digest.Sha256`,
  `Test.Pass` -- failed with "declares no types" unless the same step also
  wrote the import, which the auto-import preamble never does because it
  runs as a step of its own. Sessions now carry imported type names the way
  they already carried values, so every stdlib module that declares a type
  is usable from the REPL. `Test` had the defect since it gained
  `TestOutcome` and nothing reached for it
- **`Base64.decode` reported a length error for a bad character.**
  `Base64.decode "Zm9v!"` is both five characters long and not base64, and
  it answered about the length. The character scan runs first now: both
  things are true of that input and only one tells the caller what to fix
- **The reference listed `CIDR` as unordered while also listing it among the
  ten ordered types.** `10.0.0.0/8 < 192.168.0.0/16` typechecks and always
  has; only the sentence was wrong. `Path`, `Glob`, `URL`, `Regex`, `Bool`
  and the containers stay out

## [0.60.0] - 2026-09-05

### Changed

- **`Env.args` is now `Proc.args`, and carries no effect.** Two things were
  wrong. `Env` is documented as "reads or changes environment variables",
  which argv is not -- it is handed to the process, from a different place.
  And the effect itself: `Env.get` must carry one because `Env.set` exists
  and two reads in a run can disagree, but argv is fixed before the program
  starts and never changes. Reading it touches nothing. So `Proc.args` is
  `Unit -> List String`, there is no `Proc!args` operation for a handler to
  intercept, and a script that only read arguments now needs no manifest
  label at all: `uses {Env, FS.Read, IO}` becomes `uses {FS.Read, IO}`

### Added

- **`Proc.pid`**, the process id the operating system assigned. `Unit ->
  Int`, and no effect for the same reason as `Proc.args`. A `Par` worker is
  a domain rather than a second process, so every branch reads one pid
- **What earns a label**, a section of the reference that says which of
  three things justifies an effect: reach outside the program,
  non-determinism inside one run, or control flow. It is what the two
  additions above are decided by, and what `Raise` being absent from a
  manifest has always been decided by

## [0.59.4] - 2026-09-05

### Changed

- **The README drops "A release binary, by hand."** It held a `curl` line
  pinned to v0.22.0, which stayed there through 37 releases. The assets are
  still on that release, so the commands worked and installed a binary that
  was 37 releases old, with nothing to say so. The install script and the
  Actions step both resolve the release themselves, and `WAND_VERSION` pins
  one where that is wanted. What the section said and the other two did not
  -- which platforms have a build, and that a `.sha256` sits beside each --
  moves up beside the one-line install
- **A multi-line backtick string starts on the line of its `=`.** The
  opening backtick leaves that line as unfinished as a bare `[` does, so it
  now counts as a bracket, and a bracket is written where the value begins:
  `let doc = \`` rather than a `let doc =` with a lone backtick on the line
  below. The lines under it are the string's own content, so the break that
  used to land there moved that text one line down the page and said nothing
  in its place. The text inside a backtick string was never altered -- only
  the position of the literal on the page. Three files in the corpus each
  get a line back
- **The stdlib's doc examples write JSON and TOML between backticks.**
  Twenty-one `>>` lines held a document as an escaped string --
  `JSON.parse! "{\"a\": 1}"` -- where the backslashes are wand's syntax and
  the reader has to take them back out to see the JSON. They read
  ``JSON.parse! `{"a": 1}` `` now. The expected-output lines below them are
  unchanged: those are what wand prints, and it prints a `String` with
  escapes

### Fixed

- **A `match`/`handle` arm whose body ends in a nested match keeps its
  bracket.** An arm ends only where the next `|` begins, so a bare match
  printed at the end of one takes the arms below it. Whether the body ends
  in a match was decided by following `let` tails only. `with`, `fn` and
  `if` end just as openly, so
  `| S!n d k -> with k as _ -> match ... with | e -> ""` gave the handler's
  next operation to the nested match. The result is a different program that
  the formatter prints as a fixed point, so a check that only asks whether
  formatting settles could not see it -- the daily fuzzer reported it as
  `format:unstable` because the reparse also respelled `S!e` as `S! e`
  (#22). An `if` with no `else` prints its *then* branch last, which is a
  fourth shape, and all four are now followed

## [0.59.3] - 2026-09-04

### Changed

- **An unreachable equation is quoted and located.** The message counted
  equations and named nothing else, so `equation 2 for 'f' is unreachable`
  left the reader counting lines to find the one it meant. It now quotes the
  equation as written and carries the position every other diagnostic
  carries: `2:7: equation 'f 1' is unreachable`. The fold into a `match`
  had dropped each equation as a syntactic unit, so there was no location
  left to report; the parser now keeps the location of an equation's first
  pattern and the fold wraps it around that equation's body. The name is the
  one the equation was written under, where a local function inside another
  one used to report its host's name -- which, in a message that quotes the
  source, would have been a quote of something nobody wrote
- **Equations that do not cover every case report where the group starts.**
  `the equations for 'f' do not cover every case, e.g. _` carried no
  position at all, for the same reason: the fold left nothing to point at.
  No one equation leaves the gap, so the message now points at the first --
  `1:7: the equations for 'f' ...`. A hand-written `match` is unchanged; it
  sits inside a `Located` of its own and already reported the `match`

## [0.59.2] - 2026-09-04

### Fixed

- **V-PRED3 is quiet on a function that can raise.** A name carries one
  ending. `has_example?!` does not parse. So a function that returns `Bool`
  and can raise had no name that answered both rules: `!` drew V-PRED3, and
  `?` drew V-BANG1. The `!` wins. V-BANG1 said so already in its own message
- **A binding written `in` among a block's statements takes the `;`.** The
  spelling follows where the binding stands. This asked only whether the body
  was a block, so a binding written `in` with statements above it kept the
  `in`, and one block held both spellings. Brackets of its own do not change
  where it stands: `(t; (let f = e in ()))` loses them, and the binding is
  the block's. The first statement of a `( ... )` is the exception.
  `(let f = ... in f ())` is a parenthesis around one expression
- **A `with` body opens its bracket on the `->` line**, as a binding's value
  does. The bracket used to take a line of its own. There it says nothing,
  because the items sit at the same column either way. Eleven files each get
  a line back

## [0.59.1] - 2026-09-04

### Fixed

- **A version keeps its brackets before a field access.** A literal that
  the `.` after it would run into is bracketed, and whether a version needed
  that was decided by its last character. A prerelease is dot-separated,
  so `1.0.0` was bracketed and `1.0.0-a` was not. `(1.0.0-a).f` became
  `1.0.0-a.f`, which is one version literal. The field access became a
  literal, and a file that was a type error typechecked. A version is now
  bracketed whatever it ends in. Daily Fuzz #20
- **A qualified constructor keeps the bracket that holds its payload.** The
  parser builds `p.M N` as `App (Qualified (p, M), N)` and `p.M(N)` as
  `Qualified (p, App (M, N))`. 0.55.3 called these one program with two
  spellings and flattened the second into the first. They are two programs.
  The bracket puts the payload inside the module: `foo.Boxed(Red)` reads
  `Red` as `foo`'s constructor, and `foo.Boxed Red` looks for `Red` outside
  `foo`. So every `p.M(N)` was written as `p.M N`, and the payload changed
  scope. The formatter also did not settle: `n d.M(N)(s [])` became
  `n d.M N (s [])`, then `n (d.M) (N (s []))`. That is what the fuzzer
  reported. The bracket is kept now, and a call is no longer flattened
  through a module's name. Daily Fuzz #21

## [0.59.0] - 2026-09-04

### Changed

- **`wand f` writes one spelling for a binding, not the three that parse.**
  A binding may be joined to its body by `in`, by the `;` of a block, or by
  the newline that ends its right-hand side. All three parse, and the
  formatter used to print each back as itself -- except the newline, which
  it printed as a `let ... in` chain wearing the block's own parentheses. So
  a block written one way came back holding two forms:
  `(let a = e` and `IO.println a` below it became
  `let a = e in (IO.println a; ...)`. The spelling is now chosen by where
  the binding stands: the `;` of the block it is in, and `in` where it names
  a value for a single expression. A body of several statements is a block
  whether or not it was written with parentheses, so `let x = e in (a; b)`
  comes back as `(let x = e; a; b)` -- the form the style guide asks for.
  The one `in` the formatter leaves alone is the one that narrows:
  `(let x = 1 in x + 1; 9)` gives `x` to `x + 1` and to nothing below it,
  which is meaning rather than spelling
- A newline separates two statements inside a `( ... )` block, so `;` between
  them is optional on the way in and written on the way out. It used to be
  required for statements and optional only for bindings, which is two rules
  for one bracket. Two plain expressions still need the `;` where the block
  does not open with a binding: inside a bracket juxtaposition means
  application, and a newline cannot tell the next statement from the next
  argument
- `stdlib`, `tools/` and `test/wand/` no longer sequence with
  `let () = e in ...`. 109 of them are now the `;` the style guide keeps for
  it. The five that stay are top-level statements, where `let () = e` asserts
  that `e` is `Unit` rather than standing in for a separator
- A `;` after a `match` or `handle` arm is kept off it by a bracket the
  formatter writes. `| Error _ -> 0;` reads as part of the arm, and a reader
  had to know the `;` closed the statement above. The parse was never in
  doubt -- the arms are still owed when the `;` arrives -- so this is the
  reading, and it applies to a statement and to a binding's value alike,
  which are the two places a `;` can follow an arm

### Fixed

- **`wand f` no longer puns a field into a spelling that means something
  else.** A field whose value already carries its name is written as the name
  alone, and two shapes do not read back as the field they came from. One
  identifier inside the brackets is the payload under that name -- `B(n)` is
  `B` applied to `n` whichever way `B` is declared -- so `B(n = n)` came back
  as a different node and stopped typechecking. And on the expression side a
  pun standing in front of a named field is the record update `T(r, b = 3)`,
  so `M(a = x, b = "z")` came back as an update of `x`; against a free
  variable it even typechecked, and built something else. A construction now
  puns all its fields or none of them, and neither side puns a lone field. A
  pattern still puns beside a named field, having no update form to collide
  with. `demos/07-jq-typed/crashlooping.wand` was the one this reached: its
  `Meta(name = name)` became `Meta(name)`, and the demo went on passing
  because nothing ran the formatter over it
- **`check_fmt` covers `demos/` and `tools/`.** They were outside the gate,
  which is why the above sat there unseen. 114 files checked becomes 132
- **A binding written with a newline stops at the definition below it.** The
  loop that took its body asked only whether another expression started, and
  after the last one the next definition did. So every definition under it
  went inside it: a file whose first function was written this way ended up
  with its whole tail nested in that function, and a `main! ()` inside a
  function nobody calls is a script that prints nothing and exits 0.
  `wand f` then wrote that reading back over the source. A statement
  starting left of the binding's own `let` is not part of its body

## [0.58.0] - 2026-09-04

### Changed

- **`:d` is the one way to ask what a name is, and `:v` is retired.** `:d`
  with no name lists the session's bindings and modules; `:d List` lists a
  module's members and their signatures, as `wand d List` does; `:d
  List.map` shows that function's doc, as it always did. `:d List` used to
  answer `List : <namespace>` and `List: no doc` -- two lines telling a
  reader nothing they could act on -- and the listing lived under `:v`,
  which shared a letter with `wand v` and meant something else by it. `:v`
  and `:env` still answer, with a line saying where their two jobs went, and
  neither is offered by completion any more

## [0.57.1] - 2026-09-04

### Fixed

- **`try` discharges a raise across a function.** `try` answers a raise with
  a `Result`, and it did so only when the body's effects were already known.
  A wrapper takes the body as a parameter, and a parameter's effects are an
  open set with nothing known in it, so taking `Raise` out removed nothing:
  `let attempt f = try f ()` answered a `Result` and still said it could
  raise, and the caught raise leaked to every caller of the thing that
  caught it. The same split `handle` got in 0.57.0 now applies here, so
  `attempt : (Unit -> 'a ! {Raise | 'e}) -> Result String 'a ! 'e`
- **V-BANG1 stops naming a function for a raise its caller brings.** With
  the above, a `try` wrapper's argument carries `Raise` and its result does
  not, and `type_raises` read both -- so it told `attempt` to call itself
  `attempt!`, which is the opposite of what it is. Argument positions no
  longer count, and positions flip through them, the same rule the manifest
  took in 0.57.0
- **`Test.test` and `Test.group` say what they always did.** Both wrap the
  body in `try`, so both now report `{Raise | 'e}` on the body and nothing
  on the result. No call site changes; the signature stopped understating
  what these two do

## [0.57.0] - 2026-09-03

### Added

- **`Random`, and a ninth effect label.** `seed int float chance? choose
  shuffle hex`. The vocabulary was eight labels and a promise that a reader
  could hold all of them at once, so the ninth had to earn its place: a draw
  is not visible in the type, two identical calls answer differently, and
  the only other way to say so was to fold it under `Clock` -- which reads
  as "waits" wherever a manifest is hovered, and would be wrong for the
  whole module. `Random` is the fourth label that leaves the world alone and
  still has to be declared, beside `Raise`, `Proc` and `Clock`
- **`Random!below`, `Random!float` and `Random!seed`.** A handler answers
  draws directly, so a shuffle can be fixed in a test rather than seeded and
  hoped over: `handle thunk () with | Random!below _ k -> k 0` takes the
  first thing offered every time
- **A handler discharges what it answers, even across a function.** A
  wrapper worth reusing takes the body as a parameter, and a parameter's
  effects are an open set with nothing known in it -- so removing a label
  removed nothing, and the wrapper came out as `(Unit -> 'a ! 'e) -> 'a !
  'e`. One variable stood for what went in and what came out, and a caller's
  effect passed straight through the handler that existed to stop it. The
  tail is split instead: `(Unit -> 'a ! {Proc | 'e}) -> 'a ! 'e`, which is
  the shape `Par.timeout` has been written by hand with all along. A thunk
  that performs nothing still passes: a row that stands for any effects
  stands for none of them too
- **A demand is not a deed.** An effect on an argument's arrow is something
  the caller may bring, and the manifest counted it, so a file could not say
  it had mocked an effect away without declaring the effect it had mocked.
  Argument positions no longer count, and positions flip through them: a
  thunk the file *builds* and hands over still counts, because the file is
  what reads the clock
- **Nothing in `Random` raises.** `choose []` is `None`, `shuffle []` is
  `[]`, `hex 0` is `""`, and `int 6 1` is the same six faces as `int 1 6`.
  An empty range is a question about the argument, and the caller already
  has the argument. `int` includes both ends, because a die has six faces
  and `Random.int 1 6` should roll one

## [0.56.0] - 2026-09-02

### Added

- **A module for every built-in type.** Seven types had a literal, an order
  and a place in the type checker, and nothing to read them with. `URL`,
  `Version`, `Glob`, `IPv4` and `CIDR` now have one
- **`URL`.** `scheme hostname host port username password origin path query
  query_list fragment to_string of_string`, the `with_` setters for each
  part, `join` for resolving a reference against a base, and `encode` /
  `decode`. Every accessor is total: the value is a URL already, so a part
  it does not hold answers `""` or `None`. `port` is an `Option` rather than
  a filled-in default, which follows from the scheme and is not this
  module's to know. The module does not normalize -- it keeps the scheme,
  host and escapes as written, where a browser's parser would lowercase the
  first two
- **`Version`.** `major minor patch core prerelease build stable? to_string
  of_string of_parts bump_major bump_minor bump_patch with_prerelease
  with_build`, held to Semantic Versioning 2.0.0. No `compare`: the language
  already orders versions by the spec's precedence rules, and a second
  answer to a settled question is one too many
- **`Glob`.** `matches? base to_string of_string`. `FS.glob` says which
  files a pattern selects and needs `FsRead` to say it; whether a path is
  one the pattern would select needs no effect, and could not be asked --
  the matcher was welded inside the builtin, behind the effect. It is lifted
  out and both go through it, so a walk and a predicate cannot disagree
- **`IPv4` and `CIDR`.** `octets to_int of_int private? loopback?` and
  `contains? network prefix first last count of_parts`. `first` and `last`
  are the addresses the prefix bounds, not "first usable host" and
  "broadcast" -- conventions of particular sizes, which a `/31` on a
  point-to-point link does not have
- **`String.to_glob` and `Decode.glob`.** A `Glob` could not be built from
  text at all: the literal was the only source, and it cannot spell a
  pattern beginning with a bare name, since `src/**/*.ts` reads as a
  variable called `src`
- **`V-PRED3`.** A function returning `Bool` and not named with a `?`. The
  `!` convention has been checked in both directions since a signature could
  say whether a function raises; `?` had only the one, so a predicate could
  go unmarked

### Fixed

- **A constructor can build every value of its own type.** `String.to_url`
  and `String.to_version` decided by asking whether the text would read as a
  single literal in source, which made a rule about writing one into a rule
  about the values. A URL literal ends at a `,` and a
  `;` because they are the punctuation around it, and both are legal in a
  URL -- so `https://x/s?tags=a,b` could not be built, nor an IPv6 host,
  whose brackets end a literal too, nor a version carrying build metadata,
  since `+` is the addition operator. `Decode.url` and `Decode.version` had
  it as well, where a document the program did not write is exactly what
  holds a `,` in a query string or a `v` on a git tag. Each grammar lives in
  one place now, checked by the literal, the constructor and the decoder
  alike, as the port range has always been
- **A version with build metadata compares instead of raising.** The
  prerelease was found by splitting on `-`, so `1.2.3+b` left a `+` where a
  number was expected and the read failed. Nothing had reached it, because no
  constructor could build such a value. Build metadata is taken off first
  now, and takes no part in precedence, which is rule 10
- **`String.to_url` answers about the URL.** `ftp://x` came back "a comment
  is `-- ...` to the end of the line, not `//`" -- true of wand's comments,
  and nothing to do with the URL
- **A malformed glob reports rather than raising.** `FS.glob` compiled its
  pattern with an engine that raises on an unclosed character class. A
  literal cannot hold one, so nothing had reached it; `Glob.of_string`
  refuses one, by compiling the pattern and discarding the result

### Changed

- **`List.all` and `List.any` are `List.all?` and `List.any?`.** They were
  the only two of 354 exported signatures returning `Bool` without a `?`
- **`--strict` fails on an unmarked predicate.** `V-PRED3` is a violation,
  as every `V-` rule is; without `--strict` it is a warning
- **`01.2.3` and `1.2.3-alpha_1` are not version literals.** A numeric
  identifier has no leading zero and `_` is not in the identifier set;
  semantic versioning admits neither, and the literal is checked against the
  same grammar as `Version.of_string`. `1.2.3-0a` is still a version --
  only a *numeric* identifier is barred from a leading zero
- **A character that cannot appear in a URL is a lex error.** `|`, `^` and
  the rest have to be percent-encoded, which was always true; the literal
  admitted them and the checked constructor would not have. In both literals
  the tightening is one decision: a literal must not be able to write a
  value the checked constructor would refuse
- **The CI workflows run their actions on Node 24.** The repository is OCaml
  and sets up no Node of its own, so this is the runtime GitHub's bundled
  actions run on

## [0.55.5] - 2026-09-01

### Fixed

- **A command literal is not a bracket.** A constructor takes the bracket
  written after it, so `wand f` brackets one whose argument's bracket does
  not hold the whole of it. That question was answered by scanning
  characters, and the scan knew `"` and nothing else -- a command holds both
  quotes and brackets that mean neither, so ``(e `"`)`` read as an unclosed
  string and ``(e `)`)`` as a bracket that closed early. The constructor
  then took a bracket it did not need: harmless as ``(O) (e `"`)``, and
  unparseable as ``t.(O) (e `"`)``, which is what a qualified name makes of
  it. The rendering is lexed now
- **A nested pipeline keeps its brackets.** A `|>` chain too wide for one
  line breaks into a stage per line, and is read back as one
  left-associative chain -- so a stage that is itself an operator needs
  brackets, and was not getting them.
  `5 |> (f |> g)` came back as `(5 |> f) |> g`, a different program, and the
  reprint of that differed again

## [0.55.4] - 2026-08-31

### Fixed

- **`wand f` keeps an `and` group under the `let` it belongs to.** A
  binding's value runs onto the next line only where that line is indented
  past its `let`. Written after `in `, a `let ... and ...` group starts
  three columns right of the keyword above it, but it laid its own
  continuation out at that keyword's indent -- so a value that wrapped, and
  the `and` line under it, landed left of their own `let`, where the parser
  reads them as something new. A `let ... in` chain of more than one line
  takes the whole line now, as it already did after a value binding's `in`

## [0.55.3] - 2026-08-30

### Fixed

- **`wand f` keeps a comment that ends a line it writes a separator on.** An
  item that opens with an operator continues the item above it, so the `;`
  that separated the two is written back. A comment runs to the end of its
  line and swallows whatever follows, so where the line above ended in one
  the `;` became part of the comment and its text changed. 0.55.2 stopped
  that `;` growing once per pass; this stops it being written at all
- **`wand f` guards a constructor hidden behind a module's name.** A
  constructor takes the bracket written after it, and the guard that
  brackets one which would take a bracket it does not own reads the flattened
  spine. `p.M N` and `p.M(N)` are the same program built two ways, and only
  the first reached the guard as a head and an argument; in the second the
  constructor sat inside the head. `p.M(N)(9 [])` came back as
  `p.M N (9 [])`, where `N` takes the bracket and the program is a different
  one

## [0.55.2] - 2026-08-29

### Fixed

- **A constructor's brackets hold a block.** `Ctor (a; b)` was a parse
  error, though `(Ctor)(a; b)` -- the same node -- was not. Only a `,` makes
  those brackets a field list, since a construction names its fields one per
  comma. `wand f` writes an application head without its brackets, so it
  turned the second spelling into the first and wrote source that would not
  parse
- **`wand f` writes a statement separator once.** An item that opens with an
  operator continues the item above it, so the `;` that separated the two is
  written back. Where the item above was copied verbatim it already ended in
  that `;`, and a second one went on after it -- the line grew a `;` per
  pass, for ever

## [0.55.1] - 2026-08-29

### Fixed

- **`wand f` keeps the quotes on a map key that needs them.** A quoted key
  was written bare whenever it was spelled like an identifier. `{"type" = 1}`
  came back as `{type = 1}`, and `type` lexes as a keyword wherever it
  stands, so the map ended at the key. The lexer decides now, rather than a
  second keyword list kept in the formatter
- **An uppercase map key keeps its quotes too.** `{"Pod" = 1}` came back as
  `{Pod = 1}`, which the expression parser refuses. The same function prints
  an import pattern, where `let {TestOutcome, Pass} = import Test` does read
  those bare, so the caller says which parser will read the key

## [0.55.0] - 2026-08-28

### Added

- **`V-SHELL2`.** A newline inside `$()` starts a second command, as it does
  in a shell script, so a command broken over two lines for width runs as
  two. The half above the break can do its work before the half below fails.
  `\` is the shell's continuation and wand passes it through, so
  `wand t --fix` writes one at the end of the line

### Fixed

- **`wand f` keeps a shebang.** The lexer steps over `#!` on line one and
  emits no token for it, so it reached neither the parser nor the pieces the
  output is assembled from, and the formatter wrote every other line back
  without it. `wand f` writes in place, so formatting a script that runs
  itself stopped it running. Present in 0.53.x and 0.54.0
- The manifest `wand t --fix` writes stands off from the file below it. It
  was written against the first import, which is correct and reads as
  hand-patched: every file in the tree puts a blank line there, and so does
  the formatter

## [0.54.0] - 2026-08-28

### Added

- `--help` and `-h` on every command. Each prints that command's usage and
  exits 0. Two commands did not answer before. `wand i --help` started a
  session, and `wand lsp --help` started a server. Both hang. A flag after a
  script is still the script's. `--help` after `-e` is still part of the
  expression
- `wand l` as a second spelling of `wand lsp`. Every other command has a
  single-letter spelling. `wand lsp` still works, so an editor that spawns it
  needs no change

### Changed

- **`wand t` takes a file.** Give an expression with `-e` or `--expr`. The two
  cannot be told apart by shape, because `deploy.wand` is a valid path
  expression. `wand t ./deploy.wand` typechecked the path literal, answered
  `Path`, and exited 0. It reported success for a file it never opened
- **An expression at the top level is `wand -e`.** A file is still the
  positional argument. `-e` and `--expr` mean the same thing in both places
- **`wand d` answers every "what is this" question.** A name gives its type
  and doc. A module gives every name in it with its signature. No name gives
  everything in scope. `--load` still puts a file's own names in scope, so
  `wand d --load mine.wand` says what that file defines
- **`wand d --json` is always an array.** It returned a bare object for a
  single name, so a reader had to branch on the argument
- **`wand v` prints the version.** It was `wand V`
- **A missing manifest is a violation.** `A-USES2` becomes `V-USES2`. A file
  that reaches outside itself and says nothing has no line to check it
  against. A manifest wider than the file stays `A-USES1` and stays advisory.
  That one is imprecise, not unsafe. `wand t --fix` writes the missing line
- **`--strict` implies `--lint` when it runs a script.** It reports the
  findings, and it refuses to run the file if a finding is a violation.
  Advice does not stop a run. `--strict` alone used to reach the script
  untouched, so it asked for a gate and got an ordinary run. A script that
  takes its own `--strict` gets it after `--`
- `--load` beside a file is refused. It seeds a session, which a file check
  does not use. A flag that is accepted and dropped is a check that did not
  happen
- `--dry-run` and `--trace` are refused with `-e`. They report a script's
  effects, and there is no mode to give an expression

### Removed

- `wand e`. Use `wand -e`
- `wand t --file`. The file is the argument
- `wand v` as the scope listing, and `wand env`. Use `wand d`
- `wand V`. Use `wand v`

Each removed spelling reports what is wrong and names the one that works.

### Fixed

- An unknown option is named by every command that takes an argument. It used
  to be read as the argument. `wand f --nope` looked for a file of that name,
  `wand v --nope` for a module, and `wand d --nope` reported no documentation
  and exited 0
- A flag that takes a value and did not get one says which value is missing.
  It was reported as an unknown option, which names the wrong problem
- `wand t --fix` says `nothing to fix in <file>` when it changed nothing. It
  printed nothing and exited 0, which reads the same as a file it did fix

## [0.53.2] - 2026-08-28

### Fixed

- **`wand f` no longer grows a file without bound.** 0.53.1 writes a `;`
  before a top-level item that opens with an operator, so the item does not
  read as a continuation of the item above. The separator goes on the piece
  above, and a comment cannot hold one. A comment runs to the end of its line
  and swallowed the `;`. A `--` above a `-` line gained a character on every
  pass, and the comment's own text changed under it. `wand f` writes in place,
  so a file formatted twice was a file corrupted twice. Nothing is owed above
  a comment. A comment ends its line, so an operator below it continues
  nothing. 0.53.0 is not affected

## [0.53.1] - 2026-08-28

### Fixed

- `wand f` writes source that parses. Five shapes broke this. An application
  whose callee spans lines ends where the callee's bracket closes, and the
  guard counted only what the *first* line left open -- so the argument under
  a block callee, and the `in` under that, read as continuing the definition.
  `let x : T = e` is written by a branch of its own, which went around the
  helper that brackets a wrapped value. A top-level `let` lost the brackets
  that are a pattern's syntax there: `let (e.I) = ""` came back as
  `let e.I = ""`, which stops at the dot. A unary operator written onto its
  operand made one token of the two: `- ./` came back as `-./`, the float
  operator ML has and wand does not. A parameter list is read as names until
  the `->` or the `=`, and the brackets that make `t.A` a pattern in one were
  dropped: `let a (t.A) = c` came back as `let a t.A = c`
- `wand f` writes source that means the same thing. A top-level `let` reads
  its head as the name being defined, so `let (E) = []` came back as a value
  named `E` rather than a match against the constructor, and
  `let (Some x) = e` as a one-clause definition of a function called `Some`.
  A bare constructor absorbs the bracket after it, which is harmless only
  while that bracket holds the whole argument: `O (())` came back as `O ()`,
  the empty field list, and `O 2024-02-29.n` as `O (2024-02-29).n`, moving
  the field access. A decimal literal too large for a double lexes to
  infinity, and was written back as `inf` -- a variable, not a number. It is
  written back as digits that overflow again. An `import` renders as the
  keyword and then a path or a module name, and a `.` after it runs into
  whichever it is: `(import /t).s` written as `import /t.s` names a different
  file. An item opening with an operator continues the item above it, and one
  that fell back to a verbatim slice never reached the guard for that -- the
  `;` such an item was written after is kept now
- A `with` below a `try` is a statement of its own. The hint that wand has no
  `try ... with` was owed on any following `with`, including one back at the
  `try`'s own column -- so a top-level `with ... as ... -> body` under a
  `try` did not parse, though the reference gives four examples of the form
  and it runs. The layout rule decides now, as it does everywhere else: the
  hint is owed on the same line, or on a line indented past the `try`, and
  both still report it
- `wand f` settles. A comment kept whatever trailing whitespace its source
  had, so the formatter wrote a line ending `-- `, the next pass lexed that
  comment without the space, and the file alternated between the two
  spellings for ever -- four seeds found it in one night.
  `let (P(a, b)) = y` alternated between two spellings, and each
  constructor-absorption shape above disagreed with itself on a second pass
- `%{...}` ends where wand's braces balance, not where a shell's do. Every
  brace was counted, including one inside a `$(...)`, where a brace is an
  ordinary character -- so an interpolation could end early, swallow a brace
  the string meant to keep, and come back a `}` short. All three
  interpolation forms had their own copy of the loop and all three had it

## [0.53.0] - 2026-08-26

### Added

- A second fuzz oracle, over the formatter, in `test/fuzz`. On any input that
  parses, `wand f` must write source that parses, must settle after one pass,
  must keep every comment, and must leave the file meaning what it meant. The
  margin varies per input, because a layout bug is a bug about what fits.
  `tools/check_fmt.wand` asks whether formatted files stay formatted, which a
  corpus that is already a fixed point always answers yes to. This asks the
  same question about source nobody wrote. `test/fuzz/known.txt` is empty
- `fuzz --input FILE --show` prints what the formatter did to one input: both
  passes, the comments either side, and the type either side. A format
  finding says a property broke, not what came out
- `docs/llm-authoring.md`. What in wand serves work that a model writes and a
  person reads, and why the syntax is ML-style rather than Algol or Wirth.
  Rationale, not a benchmark. It says so, and it says how the claim could be
  measured. Linked from the README
- `V-SHADOW1`. A top-level name bound twice in one file reports on the second
  binding. The first is not dead, so which value the name means depends on
  the line it is read from. A binding named `_` is exempt. An inner binding
  shadows freely. Two imports binding one name stay `V-IMP1`'s. The rule
  found one in wand's own tests: two unrelated values called `base`, 120
  lines apart, of different types

### Changed

- A newline ends a statement wherever a statement can end. It used to end one
  at the top level and mean nothing inside a bracket, which is two rules for
  one piece of punctuation. Indentation decides now. A line indented past the
  statement above continues it, and a line back at its column starts a new
  one. A bracket the statement opened suspends the rule until it closes, so
  an argument list still runs down the page
- A binding inside a block ends at a newline, as one at the top level does.
  `let a = 1` and then `a` below it needs no `;` and no `in`. Both spellings
  still work, and `wand f` prints back the one that was written
- A definition runs onto an indented line without being bracketed first.
  `let y = f` and then an indented `1` is one application. It used to be a
  parse error asking for brackets. A function's next equation still ends the
  body above it however far in it is indented
- The bracket a constructor or a type declaration takes obeys that rule too.
  This changes what three spellings mean. A `(` back at the declaration's own
  column opens the next item, not a field list. `type Foo` with `(x: Int)`
  below it at column one now names the missing `=`. `type Colour = Red` above
  a line opening with `(` is two items. `H` above `("s" H)` is two items.
  Indent the bracket and it reads as the payload it always did

### Fixed

- `wand f` no longer costs exponentially in nesting depth. The emitters laid
  a value out to measure it, then laid it out again at the indent it would
  really sit at, and both walks asked the same question of every child. A
  `let ... in` chain fourteen deep took nearly two minutes at a forty-column
  margin. Layouts are cached per item, keyed by the expression itself
  compared by identity, its indent, its column and the margin. The two places
  that measure at a different indent ask flat instead, where nothing can
  wrap. A 7.4KB file that took 5.2s takes 0.01s, and depth one hundred
  finishes in hundredths
- `wand f` writes source that parses. Six shapes broke this. A wrapped `if`
  condition left the `then` attached to nothing. A wrapped `try` did the same
  to whatever keyword followed it. A bracket written onto a glob makes the
  sequence the lexer reads as an attempt at a block comment, at five separate
  writers -- the hazard belongs to the bracket, not to any one of them. A
  glob holding an unmatched `[` swallowed the rest of the file; it is a
  literal `[` now, as it is to fnmatch(3). A URL swallowed the `;` that ended
  its statement. An item opening with an operator read as a continuation of
  the line above. `wand f` writes in place, so each of these corrupted the
  file it was asked to tidy
- `wand f` writes source that means the same thing. A field access lost the
  brackets that made it one: `(6).o` reads back as a float missing its
  fraction, `(S 6).o` as `S (6.o)`, and `(./p).log` as a single path. The last
  one typechecks, so nothing downstream said a word. A bare constructor
  absorbs a following bracketed expression and nothing else, and the
  formatter bracketed by position instead -- so `(f S S) (B m)` came back
  reading two constructors as one application, and a qualified head did the
  same with no constructor argument at all. `$(i)` runs the command `i` and
  `$ (i)` runs whatever the value `i` holds; both printed as `$(...)`.
  `2222222.5` printed as `2.22222e+06`, a number in a spelling wand cannot
  read
- `wand f` settles. A `handle` with no arms grew a blank line under it on
  every pass, so a file grew without bound. An item whose last token spans
  lines opened a gap the next pass filled: the assembler read the item's last
  line off the line that token started on, and a raw string holding a newline
  is the case where those differ
- `wand f` keeps every comment where its author put it. Whether an interior
  comment survived was decided by searching the output for its text, so a
  rendering that had dropped the comment `--` still satisfied the count by
  holding the string `"--"`. The count lexes the rendering now. The formatter
  also clears its comment state however an item ends, so one file is never
  laid out against another's comments -- which the language server, holding
  one process open, could reach
- A local multi-clause function keeps its clauses. Each one repeats `let` at
  the binding's own indent. Aligning the later clauses under the first one's
  name parses only where a newline ends an expression, so the same function
  inside a `( ... )` came back as a parse error. `stdlib/List.wand` is the one
  file in the repository whose formatting changed

## [0.52.0] - 2026-08-26

### Added

- A fuzzer, in `test/fuzz`. It reads every `.wand` file under `stdlib/`,
  `test/wand/` and `examples/`, edits copies of them in memory, and checks
  that a typecheck of the result answers with a diagnostic. It never writes
  to the files it reads. An escaping exception, a stack overflow or a hang is a
  finding. It shrinks what it finds, confirms it in a fresh process, and
  writes one reproducer per distinct signature. `--seed S --only I` replays
  a finding exactly. `.github/workflows/nightly-fuzz.yml` runs it nightly on
  four seeds and files an issue for each new signature
- `test/fuzz/regressions/` holds a reproducer for each fuzz finding that is
  fixed. `dune test` runs them on every PR

### Fixed

- An import that cannot be loaded reports as `E-IMPORT`, at the line of the
  import. Every way module loading can refuse -- a module this binary does
  not carry, a file that is not there, a symbol or constructor a module does
  not export, a bare `import` that binds nothing, a pattern that cannot
  destructure one, an import cycle -- raised `Failure`, and arrived as
  `E-FAIL` with no code of its own and no position. `wand t` could not point
  at the line and the language server could not underline it. Found by the
  fuzzer, eight ways
- A type with more than 26 type variables prints them all as type variables.
  The count was added to `'a`, so the 27th printed as `'{` and the 159th
  crashed. A function of 180 arguments turned `wand t` into a crash report.
  Names wrap now: `'a` to `'z`, then `'a1`. Found by the fuzzer

## [0.51.0] - 2026-08-26

### Added

- A type lens above each definition. `wand lsp` answers
  `textDocument/codeLens`. Each lens gives the inferred signature of one
  value. A `type` line gets no lens, because it already says what it
  declares. When a line binds more than one name, each lens shows its name.
  The `editor.codeLens` setting turns lenses off

### Fixed

- A label in `uses {...}` hovers as the effect it is. The hover showed `Env`
  and `IO` as the modules they also name. A manifest declares effects, not
  modules. It showed nothing for `FS.Read`, `Raise` and `Proc`, because they
  name no module. The parser now gives the extent of the manifest. A label
  reads as an effect in the manifest, and as a module elsewhere
- A doc example in a hover keeps its answer on its own line. The editor reads
  a doc string as markdown. `>> ` starts a blockquote, so the answer joined
  the paragraph above it and the prompt disappeared. Examples now go in a
  fence, split by the same reader `wand d` uses. A hover shows the transcript
  that `wand d` shows

## [0.50.0] - 2026-08-25

### Added

- `wand a.wand --lint` reports the lint findings, then runs the file. A lint
  is not a type error and not a compiler error — a file that earns one still
  runs correctly by the language's own rules — so a plain run does not lint
  and still does not. This is how the verdict is asked for on the path that
  runs the file, which is where a rule like `V-IMP1` describes something the
  reader is about to be surprised by. Findings go to stderr, so stdout stays
  the script's
- `wand a.wand --lint --strict` makes a violation a failure, and a failure
  does not run — the promise `wand t --strict` already makes. `--strict` is
  wand's only beside `--lint`; on its own it reaches the script untouched, as
  every other subcommand's flag does
- `WAND_MAX_CALL_DEPTH`, how deep calls may nest before a run is refused.
  Default 1,000,000. Lower it when the stack is smaller than the default —
  under `OCAMLRUNPARAM=l=...` or a small `ulimit -s` — because a bound above
  what the stack can carry never fires; raise it for a script that genuinely
  nests deeper

### Changed

- **A call that nests deeper than 1,000,000 is refused.** A call with work
  waiting on it keeps a stack frame, so nesting without end used to exhaust
  the stack and kill the run with `Fatal error: exception Stack overflow` —
  which cannot be caught: a handler that matches it, even one whose guard
  rejects it, hangs rather than unwinds, because the guard runs on the stack
  that just ran out. The depth is bounded before the stack goes, and the
  refusal is a wand error a script can catch. Only a call with work waiting
  on it counts, so a call in tail position still runs to any depth.
  A non-tail recursion deeper than the bound ran before and does not now;
  reaching that depth costs time quadratic in it, because the frames are live
  roots and every minor collection rescans them, so the code this rejects was
  already paying for the depth. `WAND_MAX_CALL_DEPTH` raises it

### Fixed

- An expression that answers Unit without performing anything prints its
  answer. `()` was silent, and so were `let u = () in u` and `if c then ()
  else ()`. Every Unit was suppressed, which is right for `IO.println "hi"`
  — the line is already on the screen, and `() : Unit` under it is noise —
  but the suppression keyed off the value, and a unit the user asked to see
  has the same value as one a call handed back. It now keys off the effects
  the expression performed, which is the question actually being asked
- An effect in an imported module's top-level binding runs. `let greeting =
  $(hostname)` at the top of a module killed the run with a crash naming an
  unhandled effect. The cause was ordering, not policy: imports
  were evaluated before the handler was installed, in every run path. The
  module's bindings now run under the handler a script's own body runs under.
  Manifests are unchanged — a module whose `uses` is narrower than what it
  does is still refused
- An operation with no handler comes back as a wand error naming it, rather
  than as a raw crash report. The handler's cases end in a catch-all, so an
  unknown name or a payload of the wrong shape reached it
- A bare constructor that swallowed an argument is corrected, not just
  reported. Parentheses after a constructor are its payload whatever its
  number of fields, so `f None (1)` is `f (None 1)` and the argument meant
  for the call went to `None`. `wand t` knew it took none and said to write
  `(None)`; `wand t --fix` and the editor's code action now write it. What
  is parsed is unchanged — counting the fields there is what made
  `Ctor (a, b)` mean different things in different files

## [0.49.0] - 2026-08-25

### Added

- `T.parser`, holding everything reading a command line takes: the account of
  the flags, the reader, and the usage line. `Args.read Opts.parser (Env.args ())`
- `CommandLine`, the built-in record type `T.parser` answers with. A file can
  take one apart and can build one
- `CIDR` is an ordered type. `9.0.0.0/8 < 10.0.0.0/8`, and `Ord` is ten types

### Changed

- **`Args.read` takes one argument where it took two.** `spec` and `reader`
  were derived separately and only ever used together, and nothing said they
  had to come from one type. `Args.read B.spec A.reader argv` typechecked and
  failed during the run with a message about the arguments. A `Map String`
  carries no trace of the type it came from, so the pairing could not be
  checked anywhere
- **A value cannot take the name of a type or a constructor.** `type
  Pod(host: String)` beside `let Pod = 1` was accepted, and the value could
  not be reached from anywhere. A name declares one thing
- The error at a field access on a type nothing has pinned names the
  annotation to write, and the declared types that have the field. `let
  port_of p = p.port` says `'p' needs its type before '.port' can be read:
  write '(p: Pod)'`

### Removed

- `T.spec` and `T.reader`. Naming either says to write `T.parser`, which
  holds both and the usage line

### Fixed

- A lambda written before the argument that says what its parameter is could
  not read a field off it. `List.map (fn p -> p.host) pods` was a type error,
  as were `filter`, `sort_by`, `fold_left` and every pipe form. Every
  higher-order function in the standard library takes its function first.
  Arguments are still read in written order, and a lambda's body now waits
  until the rest have been read
- A lambda applied where it is written had the same fault, one position over.
  `(fn p -> p.port) pod` could not see the argument under it
- `List.unique` did not answer with the equality `==` answers with.
  `2026-08-25 == 2026-08-25T00:00:00Z` was true, and `List.unique` of the two
  returned both. Membership compared the stored value, so it compared the
  spelling. The same for `60s` and `1min`, and for `1000KB` and `1MB`
- `List.sort` compared a `CIDR` as text, so `10.0.0.0/8` sorted below
  `9.0.0.0/8`, the two addresses the opposite way round from what `IPv4`
  answers
- **`List.sort` ordered a variant by constructor name.** `type S = Zulu |
  Alpha` sorted to `[Alpha, Zulu]`, and renaming a constructor moved values.
  A constructor sorts where it was declared. `Option` and `Result` declare
  their absent and failed cases first, so `None` before `Some` and `Error`
  before `Ok` are unchanged

## [0.48.1] - 2026-08-25

### Fixed

- A field could not hold a decoder. `type D(decoder: Decoder Pod)` could not
  be built from `Pod.decoder`. A field's type reader turned `List`, `Map` and
  `Result` into the forms the checker uses, and left `Decoder` as a plain
  application, so nothing could fill the field. The error said `Decoder Pod
  and Decoder Pod are not the same type`
- A written signature did not bind the parameters before the body was read.
  `let get : Box 'a -> 'a = fn b -> b.v` said `field access requires a named
  type, got 'a`, although the signature one line up said what `b` was. A
  signature over a lambda binds each parameter first now. A lambda with more
  parameters than the signature has arrows still infers whole, so a wrong
  signature is still caught

## [0.48.0] - 2026-08-25

### Added

- A type and a constructor take the module's name: `Test.TestOutcome`,
  `Test.Pass "x"`, `| Test.Pass s ->`. This works in a type, an expression
  and a pattern. A standard library module is `Test`. A file is `one`. Both
  spellings work
- An uppercase name in a destructured import selects a type or a
  constructor: `let {TestOutcome, Pass} = import Test`

### Changed

- A type belongs to the module that declares it. Two modules can each declare
  a type called `Status`. They declare two types. A file can use both. Mixing
  them is an error: `expected one.Status, got two.Status`. This used to
  typecheck. One of the two types won, and nothing said which
- **An import brings only what it names.** A module's types and constructors
  used to arrive under their bare names. Reach them through the module, or
  name them in the import. A file that writes a bare imported name now fails
  with "unknown type" or "unknown constructor". Twenty-six places in this
  repository needed the change
- Renaming a type in an import also renames its constructor, where it has
  one. `let {Conf = MyConf} = import ./foo` gives `MyConf(port = :80)`.
  Renaming one constructor out of several is refused. The error says to
  rename the type, or to use the module
- An alias names what its target names. `type MyConf = Foo.Conf` builds and
  matches. An alias used to name no constructor. An alias to a type with
  several constructors is still not a value
- A type error prints the short name. Where two names print the same, both
  take the module
- `wand f` prints an uppercase key in an import without quotes

### Fixed

- The compile cache keyed on a module's source and its dependencies. Two
  files with the same bytes at two paths shared one entry, which gave one
  file the other's type names. The key includes the path
- One module reached by two spellings of its path was two modules. A module
  key is normalised

## [0.47.0] - 2026-08-24

### Added

- A type describes a whole command line. The flags are a record -- each has a
  name and a type -- and what is written without a flag in front of it has no
  name at all, so a type with one field whose type is a record and one that
  is not describes both halves. Which field is which comes from the types
  rather than the names
- `T.reader`, the decoder that reads a command line rather than a document,
  and `Args.read` to run it. The argument field's type says how many there
  may be: `String` exactly one, `Option` one or none, `List` any number.
  Each is read as its own type, and a refusal names the field --
  `.host: expected one host, got 2`
- `T.spec`, what a flag's own text cannot say: a `Bool` field is a switch
  taking no value, and a `List` field collects rather than replacing. Both
  reach the reading from the type instead of a list written beside it
- `T.usage` covers the arguments as well as the flags:
  `[--port :8080] [--verbose] <host>`
- A repeated flag collects. `--tag a --tag b` is two tags where
  `--name a --name b` is one name written twice, and a flag that collects
  holds a list however many times it was written, including none
- `Args.help?`, which answers `--help` before the arguments are read. It is
  false after `--`, where a `--help` is an argument like any other
- `V-IMP2`: an import that binds nothing the file mentions, with a fix that
  deletes the line. It found fourteen dead imports here, six of them
  `import Option` lines that died when `Option` became built in

### Changed

- `--` ends the flags. What follows is positional whatever it looks like,
  which is the only way to pass an argument beginning with two dashes.
  `Args.parse raw ["--", "-x", "y"]` used to answer `Ok(["y"])`, reading `--`
  as a flag with no name that swallowed the next argument
- Reading a command line that asks for help says what to do about it:
  `--help expects a value; \`Args.help?\` answers it instead`

## [0.46.0] - 2026-08-24

### Added

- A field may declare a default: `type Conf(host : String, port : Port =
  :8080)`. A construction may leave that field out, and `Conf()` builds one
  where every field has a default. A default is a value written out -- a
  literal, or a constructor applied to literals -- so it reads with nothing in
  scope, says the same thing at every site that omits the field, and performs
  no effect a construction would have to declare
- A derived decoder reads defaults too: a field the document does not carry
  takes its default rather than failing, and a field it does carry wins
- `T.usage`, the command line that reads a type, derived beside `T.decoder`
  and `T.encoder`. A field with a default prints bracketed and shows the
  default; an `Option` field prints bracketed and shows what the flag takes;
  a `Bool` prints as a switch; anything else is required and shows its type.
  The usage line and the decoder now come from one declaration, so a flag
  cannot be in one and missing from the other
- `wand t --fix` inserts a missing import. The checker already named the
  module, so the line rides with the error: it goes under the manifest, joins
  the run of plain imports in the order that run is kept, and stays above any
  destructured `let {a} = import X`

### Changed

- `Option` is a built-in type. Its name and its `Some` and `None`
  constructors need no import, the way `Result`, `List` and `Map` need none.
  The module keeps its functions, and calling one still needs `import
  Option`. **Declaring `type Option` is now an error**, as it already was for
  any other built-in name
- A `Bool` field a document does not carry reads as `false`, the way an
  `Option` field has always read as `None`. `Args.parse_with ["verbose"]`
  failed on every command line that left `--verbose` off unless the field
  carried a default. A default still wins over both
- A constructor that declares one field name twice is an error. It was taken
  silently and the first won, so the second's default never applied and its
  type was never checked against anything
- Empty parentheses read the same way in a pattern as in a construction:
  `C()` names no fields where `C` has them, and is the constructor that
  carries nothing where it does not
- The declaration errors state what happened and stop; the clause after the
  semicolon is gone from each

### Fixed

- Six declaration errors reported line 1, column 1 whichever line the
  declaration was on. They now point at the thing the message names -- the
  type name in the *second* of two declarations, the repeated constructor,
  the repeated field, or the default expression itself
- A keyword where a field name goes says which word is the problem.
  `type Run(result : T)` reported "expected type name, got result", which
  reads like nonsense beside a word that is plainly a name

## [0.45.0] - 2026-08-24

### Added

- A field puns. `Pod(name, restarts)` binds each field to a variable of its
  own name in a pattern, and builds from the names already holding those
  values in a construction. `{a, b}` has punned for a map since maps got
  braces, and a record had no equivalent, so `Pod(name = name, url = url)`
  wrote every name twice
- Which reading a list of bare names carries comes from the declaration, not
  from the spelling. A constructor that names its fields reads them as
  fields; one whose payload is a tuple reads them as the tuple, so
  `Some(a, b)` is unchanged. The space in `Pod (name, restarts)` decides
  nothing
- A pun mixes with a field that carries a value: `Pod(name, restarts = 0)` in
  a pattern, `Pod(restarts = 0, name)` in a construction. In a construction a
  bare name written first is the base of an update, which is what that
  spelling meant before puns existed, and the type error there names the
  reordering that gets the pun

### Fixed

- `type X (T, U)` left the parser's bracket count raised. The fields are read
  by trying the named form first and rewinding, and the rewind put the
  position back but not the count, after which no newline ended a top-level
  statement. A definition two lines down was read as a continuation of the
  one above it, and `wand f` wrote that reading back to the file. Every
  rewind now restores both

### Changed

- `wand f` hugs a constructor's parenthesised arguments: `Some(a, b)`, not
  `Some (a, b)`. The two forms are told apart by the declaration, so printing
  them alike is what stops the space from looking like the thing that decides
- `wand f` collapses a field that names its own value, as it already does for
  a map: `ShellResult(stdout = "", code = code)` prints as `code`

## [0.44.0] - 2026-08-23

### Added

- A `Result` module: `to_option`, `ok?` and `error?`. `Option` had eight
  combinators and `Result` none, and the gap showed up as `Env.get`,
  `Map.get` and `List.get` each writing `match ... | Ok v -> Some v |
  Error _ -> None` by hand — one function spelled three times. All three
  are written with `Result.to_option` now
- `Option.to_result` named the crossing one way only; `Result.to_option` is
  the way back, and drops the reason rather than inventing somewhere to put
  it. `ok?` and `error?` are the same question either way, so either can go
  straight to `List.filter`. Matching a `Result` stays the usual way to deal
  with one

## [0.43.1] - 2026-08-23

### Fixed

- 53 standard library functions had no example, which 0.43.0 reported as
  complete: the count was per module, so a module with one example passed
  like a finished one. 296 of 303 functions now have one, 357 examples in
  all. Most of the gap was one mistake repeated — of each `Result`/`!` pair
  the raising half was documented and the `Result` half was not
- `tools/check_docs.wand` counts per function. It ran the examples and
  checked they held, but never asked which functions had none, so it could
  not have caught the above. The seven that cannot have an example are
  listed there with the reason
- `V-BANG1` suggested a name that does not parse. A predicate that can raise
  was told to call itself `found?!`; a name takes one ending, so `?!` and
  `!?` are both parse errors. It is `found!`
- `Par.each`'s example ran two workers, both printing, so its output order
  was not guaranteed — it agreed twelve times running, which is not the same
  as being deterministic. One worker, and the doc says what order is and is
  not promised

## [0.43.0] - 2026-08-23

### Added

- Every function in the standard library has an example in its doc string,
  run by CI — 308 across 26 modules. `wand d -x <name>` prints a doc with
  its examples run in place; `wand d -t` reports only what does not hold,
  is silent when everything does, and exits non-zero if anything does not.
  Either takes a module name. `tools/check_docs.wand` is the gate
- `Shell.failed?`, the opposite of `Shell.ok?`. `List.filter Shell.failed?`
  needs no brackets where `!(Shell.ok? r)` does

### Changed

- A string is shown with the quotes it was written with, at any depth, so a
  display says what the value was: `["a, b"]` is one element where `[a, b]`
  could be one or two. What a program *writes* is unchanged — `IO.println`
  and `%{...}` write a string as its characters, and a script that ends in
  a string writes that string
- A TOML value shows as a value rather than as a document: a table like a
  map, an array showing its elements rather than `<toml-array>`. A table
  printed as a whole TOML file before, so a list of two ran over four lines.
  `TOML.stringify` and `IO.println` still give the document

### Fixed

- `List.range` was documented as excluding its upper bound; it includes it
- `Map.empty` and `JSON.of_map` recommended the `[]` map literal, removed
  in 0.18.0
- `Float.round`, `floor` and `ceil` carried prose examples that do not
  parse: a negative literal after a function name is a subtraction

## [0.42.0] - 2026-08-22

### Changed

- A tail call does not grow the stack. A function whose body ends in a call
  runs to any depth. Every step used to be wrapped in an error handler so a
  failure could be stamped with its position, and such a handler is a frame
  that stays; the position now travels in
  a cell. A tail-recursive loop over 1.6M items goes from 11,642 ms to
  323 ms, `List.fold_left` over 200k from 449 ms to 100 ms
- Deciding not to stop costs two atomic loads. Every step of evaluation asks
  whether it should stop, and the answer used to take two reads — more, on
  the shapes a script runs, than resolving all of its names. Ctrl-C still
  stops a script in about a millisecond, and a losing racer still stops
  where it stands
- A recursive call reuses the function it already has, rather than building
  a second copy of it and a wrapper to carry it in, on every call

### Fixed

- The throughput benchmark measures what it says it does. Its cons-pattern
  workload still used the `:` that stopped being cons in 0.31.0, so what it
  timed was a parse error — which reads as the healthiest line in the table,
  since a workload that does not run is a fast one

## [0.41.0] - 2026-08-22

### Added

- `JSON.of` and `JSON.of!` write any value as JSON in one call: numbers,
  text, every domain type, lists, maps, options and records, and any nesting
  of them. A structure no longer has to be converted a piece at a time —
  `JSON.of! [1, 2, 3]` where it was
  `JSON.of_list (List.map JSON.of_int [1, 2, 3])`. What cannot be written is
  a value holding code, and that is the `Error`; the `of_*` builders stay,
  precise and total
- `TOML.of` and `TOML.of!` build a TOML document. `TOML` had no constructors
  at all, so a document could be parsed and re-printed but never built from
  a script's own data. A document is a table, so a bare value says so, and a
  field that is `None` is left out — TOML has no null, and writing one would
  not read back the same

### Changed

- `CSV.stringify` and `stringify_with` take `List (List 'a)`. A cell is
  text and every value has a text form, so a row of numbers or instants
  needs no conversion first. Both stay total
- `of` is an ordinary word. It was reserved so that `Circle of Int` could be
  corrected, and that correction now fires where the mistake is written,
  after a constructor name, with the same message

### Fixed

- `:reset` in the REPL opens what a session opens with. It built its own
  list of modules and had been missing eighteen of them since they were
  added, so a reset session could not reach `Map`, `JSON`, `Test` or twenty
  others that a fresh one could, and nothing said why

## [0.40.0] - 2026-08-22

### Added

- `type X = <a type>` is an alias: another name for a type that already
  exists. `type Point = (Int, Int)`, `type Ids = List Int`,
  `type F = Int -> Int`, `type This = That`. It is transparent — the two are
  one type, interchangeable in both directions — so it buys a name to read
  and write, not a distinct type; a record is still what the checker keeps
  apart. A type shows with the alias it was written as, `Point (= (Int,
  Int))`, so the name in the source is the name in the message
- An alias takes parameters: `type Pair 'a = ('a, 'a)`,
  `type Either 'a 'b = ('a, 'b)`. They are bound to the arguments at the use
  site, so the same alias answers differently each time it is applied, and
  the wrong number of arguments says how many it takes
- `type Point = (Int, Int)` and `type F = Int -> Int` parse at all, which
  they did not before

### Changed

- **Breaking:** `Url` is `URL`. An acronym is written in capitals, which
  `IPv4`, `CIDR`, `JSON`, `TOML` and `CSV` already were. The old spelling is
  an unknown type whose hint names the new one — which meant teaching that
  hint about built-in type names at all, so `Strig` now suggests `String`
- **Breaking:** a name declares one thing. Two `type` declarations of one
  name, or two constructors sharing one, were taken silently, and which of
  them won differed between a file and the REPL. The loser stayed
  constructible and stopped being matchable, so a declared type could no
  longer be taken apart and nothing said why. A file refuses it now; the
  REPL still replaces, which is what a REPL is for
- **Breaking:** a built-in type's name cannot be declared. `type Size(a:
  Int)` was accepted, and then field access on the result answered "field
  access requires a named type, got Size" — the name resolved to the
  built-in while the constructor came from the declaration. Ten names did
  this
- A name that is a type rather than a constructor says so, instead of being
  called an unknown constructor and sending the reader after a declaration
  that is right there

## [0.39.0] - 2026-08-22

### Added

- `DateTime` is a module as well as a type: `year`, `month`, `day`, `hour`,
  `minute`, `second`, `weekday`, `day_start`, `on`, `on!`, `date_string`
  and `time_string`. Nothing in it reads a clock — `Clock.now` does, and
  this takes what it answers apart. `weekday` is ISO 8601, Monday 1 to
  Sunday 7
- `DateTime.on` is the only builder, and answers a `Result` because
  `2026 2 30` is not a day. A time of day goes on top as a `Duration`, so
  `DateTime.on! 2026 8 22 + 14h + 30min` — which needs no rule for what an
  hour of 25 would mean
- `examples/ports/rotate-backups.wand` and
  `examples/ports/provision-host.wand` finish the shell corpus: eighteen
  ports covering all twelve of the jobs it set out to cover

### Changed

- **Breaking:** there is one instant type. `2026-08-22` is a spelling of
  `2026-08-22T00:00:00Z`, so `Date` is gone as a type name and
  `2026-08-22 + 5h` moves five hours instead of standing still. The rule
  that kept two resolutions apart existed for that pair alone
- **Breaking:** an instant prints in full and in UTC, whichever spelling
  was written: `"%{2026-08-22}"` is `2026-08-22T00:00:00Z`, and
  `date_string` is the short form. Source keeps what was written — `wand f`
  leaves `2026-08-22` alone, as it already left an offset alone
- **Breaking:** `14:30:00` is not a value. `Time` had no module, no
  arithmetic and no use anywhere in the corpus; a time of day belongs to a
  day. The lexer still reads the shape and names the instant form
- **Breaking:** `Decode.date`, `Decode.time`, `String.to_date` and
  `String.to_time` are gone. `Decode.datetime` and `String.to_datetime`
  read both spellings
- Permissions, symlinks, ownership and a process surface stay out of the
  standard library, recorded in `docs/gaps.md` as decisions rather than
  omissions: they are POSIX one-liners the manifest already names, and no
  port reached for a process it had not started

## [0.38.0] - 2026-08-22

### Fixed

- A comment inside a definition no longer stops the definition being
  formatted. One comment anywhere inside a top-level item made the whole
  item a verbatim slice of the source, so `wand f` never looked at its code
  and `tools/check_fmt.wand` could not either — the better-commented a
  definition was, the less the formatter saw of it. A comment on its own
  line is now written above the match arm, block statement, list element or
  `let ... in` binding it sits on, and above the body of the definition
  when it opens one; the rest of the item is printed as usual. Ten corpus
  files held items the formatter had never formatted, and none now do
- A comment that follows code on its line still pins its item. Lifting it
  onto a line of its own would point it at the line below, so the item is
  left exactly as written. Each comment is counted in the rendered item and
  anything but exactly once sends the item back to a verbatim slice, so a
  comment cannot be dropped, duplicated or moved
- A `let ... in` arm body too wide for the arrow's line put its
  continuation at the arm's own indent, level with the `|` above it, where
  it read as the next arm. It takes the block shape a nested match takes.
  This was not caused by the change above but was hidden by it:
  `test_derive.wand` and `demos/09-fork-overhead/crunch.wand` held the
  wrapped form with no comment in sight

## [0.37.0] - 2026-08-22

### Changed

- **Breaking:** `print` and `println` are no longer in scope without an
  import. Printing is `IO.print` and `IO.println`, so a file that prints
  writes `import IO`. Every function a file calls now comes from a module it
  imported, with no exceptions — `Ok` and `Error` stay, because a
  constructor of a built-in type has no module to come from. The old
  spelling answers `unbound variable 'println' -- printing is IO.println
  (import IO)`, as `printf`, `puts` and `echo` already did

### Fixed

- Comparing two functions raises, whichever functions they are. `==` and
  `List.sort` relied on the runtime reaching the function inside, so two
  wand-defined functions whose bodies already differ compared as ordinary
  values and sorted into an order that meant nothing

## [0.36.0] - 2026-08-22

### Added

- `Clock.now` answers the current instant in UTC, and a `Duration` moves an
  instant while two instants subtract to the length between them. Two
  instants do not add, and a `Duration` does not subtract an instant: both
  are type errors that name the form to write
- `Clock.timed` runs a thunk and answers how long it took beside what it
  returned. It reads a clock no correction can move, and time while the
  machine is suspended counts
- `Test.at` pins what `Clock.now` answers, so a test of "older than thirty
  days" needs neither a real file nor a wait
- `V-CLOCK1` names `Clock.timed` when a length of time is measured by
  subtracting two readings of `Clock.now`, which a clock step spoils

## [0.35.0] - 2026-08-22

### Changed

- **Breaking:** `Par.race` refuses inside a handler. An effect cannot reach
  a handler on another domain, so the branches cannot run where they were
  written: the race answered with its first thunk and said nothing, and a
  test of racing code tested one branch and passed. The message names the
  fix — move the handler inside each thunk. `--dry-run` and `--trace` still
  run a race, left-biased, because each only reports what the work would do

## [0.34.0] - 2026-08-22

### Changed

- **Breaking:** a comment is `--` to the end of the line, and that is the
  whole form. `(*` is a lex error that names it. A comment reads no
  brackets, so pasted text survives whatever it holds — including the `*)`
  that a shell `case` arm writes
- **Breaking:** documentation is a run of comment lines directly above a
  definition, which `wand d` and an editor hover print. Each line stands
  alone, the lines are consecutive, and the last one sits on the line
  above, so a comment after code documents nothing and a blank line ends
  the run

## [0.33.0] - 2026-08-21

### Added

- `Test.with_shell_results` supplies the `ShellResult` for each `$?(cmd)` a
  test runs, so a command can exit non-zero and the failure path can be
  tested. A command the test does not name exits zero with no output

### Changed

- `Test.with_shell` and `Test.shell_calls` now stand in for `$?(cmd)` as
  well as `$(cmd)`. Both handled `Shell!run` only, and `$?(cmd)` performs
  `Shell!capture`, so a mock written for a script that inspects an exit code
  did nothing and the test passed for the wrong reason. Under `with_shell` a
  `$?(cmd)` exits zero and carries the output written for that command

## [0.32.0] - 2026-08-21

### Added

- `Port.to_int` and `Port.of_int`. `Port` could be written, ordered,
  compared, decoded and interpolated, and nothing took `8080` out of
  `:8080`, so a script could not pass a port to a command. The string form
  is unchanged and is the address: `"host%{:8080}"` is `"host:8080"`.
  `of_int` refuses a number outside 0–65535 (`2738ddc`)

### Changed

- **Breaking:** `Par.timeout` refuses to run under a handler. A race runs
  only its first thunk while one is installed, so the sleeper never ran and
  the deadline never fired — work that only the deadline would have stopped
  ran forever, hanging a test suite with no message. Put the handler inside
  the thunk instead: `Par.timeout d (fn () -> with_shell mocks (fn () ->
  ...))` keeps the mock and fires the deadline. A rehearsal and a trace are
  not refused (`7016818`)
- `wand f` keeps a value that ends in a bracket on the line that opens it,
  the way a value that is a bracket already did. Run `wand f` over a
  formatted repository once: files laid out by 0.31.0 will change
  (`9cdb6f1`)

## [0.31.0] - 2026-08-21

### Changed

- **Breaking:** `:` is no longer cons. `1 : [2, 3]` and `[h : t]` are parse
  errors naming `::`, which 0.30.0 read as well and `wand f` writes. The
  `:` still binds where cons bound, so the message is reached rather than
  "expected ->, got :" from wherever the expression happened to end
  (`5d230d6`)

## [0.30.0] - 2026-08-21

### Added

- Cons is `::`. `1 :: [2, 3]` in an expression, `[h :: t]` in a pattern.
  `:` is a type annotation or a port literal and nothing else, so the
  lookahead that told `(p: Pod)` from `(h : t)` is gone with the ambiguity
  that needed it (`043c7a3`)
- A bare `h :: t` pattern is read, and `wand f` writes `[h :: t]`. The
  brackets say list, the way `[a, b, c]` does. `Some h :: t` is
  `(Some h) :: t` (`043c7a3`)
- A pattern carries a type inside a constructor's payload: `Ok (p: Pod)`,
  and inside a tuple payload, `Ok (p: Pod, n)`. That is where a decoder's
  result lands, and it was the one place a pattern could not be annotated
  (`78e7228`)

### Changed

- **Deprecated:** `:` as cons is still read, and `wand f` writes `::`.
  It becomes a parse error in the next release. Running `wand f` over a
  file is the whole migration (`043c7a3`)
- `V-IMP1` warns on any two imports that bind one name, where it used to
  stop at the first item of anything else. Imports bind before a file's own
  bindings wherever they are written, so a use between two imports reads
  the second one — which the rule had assumed was a genuine use of the
  first (`59a1e6f`)
- A `:` in a pattern with no type after it names `[x :: xs]` (`043c7a3`)

## [0.29.0] - 2026-08-21

### Added

- A record update: `Tally(t, failed = t.failed + 1)` is `t` with one field
  replaced. The record comes first, then the fields that change. A field
  not named keeps what the base holds, and naming one twice is a type
  error. `{t with failed = 1}` is a parse error that answers with this
  form, carrying the names you wrote (`fea51a5`, `66ab3f6`)
- `FS.delete_tree` and `FS.copy_tree`, each with a raising sibling, so
  `rm -rf build/` and `cp -r` have a wand spelling. Neither follows a
  symlink out of the tree: a delete unlinks it, a copy recreates it. A
  copied file keeps its mode (`78ec907`)
- `Shell.ok? r` is `r.code == 0`, the first question a script asks of a
  `$?()` (`78ec907`)
- `Decode.map3`, for a decoder built by hand from three fields. Wider is
  `and_then`, or a derived `T.decoder` (`fea51a5`)
- `Float.format 1 0.3333` is `"0.3"`. A width is a printing decision, so
  it answers a `String` (`fea51a5`)

### Changed

- **Breaking:** six `Env` operations now say what they carry and what
  resumes them. A handler case resuming `Env!get` with an `Int`
  typechecked, and `Env.get` answered `Some(42)` where its signature says
  `Option String`. That is now a type error (`ed4f8b8`)
- A nullary constructor applied to parentheses names itself:
  `t.eq None (usage row)` said "expected Option 'a, got Option (String,
  Int) -> 'a", and now says `'None' takes no arguments` and to write
  `(None)` (`11ef36d`)

## [0.28.0] - 2026-08-21

### Added

- `Size` crosses to a number and back: `Size.to_bytes 4KB` is `4000`,
  `Size.of_bytes 6466` is `6466B`, and `Size.format` is the readable
  spelling, `"6.5KB"`. A byte count below zero reads as `0B` (`a1e7f89`)
- `+` and `-` add two `Size`s or two `Duration`s, through a new `Add`
  constraint sitting between `Num` and `Ord`: `Int`, `Float`, `Size`,
  `Duration`. `100MB + 4KB` is `100004000B`, and `1h + 30min` is `1h30m`.
  `*`, `/` and unary `-` keep `Num`. A sum of sizes is written in bytes,
  and a subtraction that would go below zero floors there (`a37d13a`)
- `List.filter_map` applies a function to every element, keeps each `Some`
  value and drops each `None` (`7fcb947`)

### Changed

- **Breaking:** `FS.size` answers `Result String Size`, and `FS.size!`
  answers `Size`, where both answered bytes as an `Int`. `Size.to_bytes`
  is the number back (`bc2ac93`)
- `wand f` writes back the binding spelling that was written.
  `(let x = 1; x + 2)` used to come back as `let x = 1 in x + 2` (`8995887`)
- `wand f` wraps a `let ... in` chain in a lambda body under the `fn`,
  where the continuation lines used to sit level with it (`8995887`)
- `wand f` closes two brackets on one line when both would close at the
  same indent (`e870013`)

## [0.27.0] - 2026-08-21

### Added

- Add a binding that lives for the rest of its block: a `let` before a `;`
  inside parentheses binds for every statement after it, so a body that
  names two intermediates costs no indentation. `;` ends the binding's
  right-hand side, exactly as a newline does at the top level of a file.
  The same three words parsed before and bound nothing — the binding took
  `Unit` for a body, and the error named the use site rather than the
  mistake. `let ... in` keeps its own meaning: it names a value for one
  expression, so `(let x = 1 in x + 1; 9)` still scopes `x` over `x + 1`
  alone. `wand f` writes the block form when more than one statement
  follows a binding, and `let ... in` when one expression does (`0e737cc`)

### Changed

- **Breaking:** A block cannot end with a binding. `(f (); let x = 1)` is a
  parse error saying the binding has no body, where it used to be accepted
  and bind nothing (`0e737cc`)
- **Breaking:** A binding that bound nothing now binds, so a program that
  shadowed a live name answers differently: `let x = 0 in (let x = 1; x)`
  was `0` and is `1`. Every other program this touches is one that does not
  typecheck today (`0e737cc`)

## [0.26.0] - 2026-08-21

### Changed

- **Breaking:** `Size`, `Version`, `Port` and `IPv4` compare. Each was a
  type error under `<`, `>`, `<=` and `>=`. A `KB` is 1000 bytes, because
  the spelling is the SI one and the lexer has no `KiB`. `Version` follows
  semver precedence, prerelease rules included, so
  `1.2.3-alpha.1 < 1.2.3-alpha.2 < 1.2.3-beta < 1.2.3`. A `Port` is its
  number and an `IPv4` is its 32 bits (`1b10b92`)
- **Breaking:** `List.sort` reads these values rather than their text, so
  it answers with a different order than before —
  `[10.0.0.10, 10.0.0.9, 10.0.0.2]` sorted to
  `[10.0.0.10, 10.0.0.2, 10.0.0.9]` and now sorts to
  `[10.0.0.2, 10.0.0.9, 10.0.0.10]`. `1.10.0` sorted below `1.2.3`, and
  `1GB` below `999MB` (`1b10b92`)
- **Breaking:** Equality reads them too, so `1000B == 1KB` and
  `01.2.3 == 1.2.3` are true. The three relations agree: a value written
  two ways is equal, and neither below nor above (`1b10b92`)

## [0.25.0] - 2026-08-21

### Added

- Add `Clock`, an eighth effect label, and `Clock.sleep 30s`, which waits at
  least that long. wand could not wait: `Duration` had literals and
  arithmetic, and nothing consumed one as a wait, so a retry with backoff
  could not be written and a hung command hung forever. One label, not
  `Clock.Read` and `Clock.Wait` — `FS` splits because a handler can grant one
  half and not the other, and a clock cannot. The sleep waits in slices and
  checks for an interrupt between them, so Ctrl-C takes effect at once and
  the brackets a script holds release. `Test.with_clock` answers the effect
  with a clock that costs no time, so a test of an hour of backoff runs in
  microseconds, and `--dry-run` reports `would wait: 45s` without waiting.
  No manifest in the tree changes: nothing performed `Clock` before this
  (`f58c626`)
- Add `Shell.timeout`, a deadline on a command. Most script hangs are a
  subprocess, and this is the deadline that kills for real. Expiry is a
  sequence — SIGTERM, a fixed five-second grace, SIGKILL — so a command that
  tidies up on TERM gets to, and one that ignores it does not keep running.
  Only a deadline produces an `Error`, and the message names the command and
  the duration. Every other failure passes through: a command that exits
  non-zero has failed, not run late. The deadline is per command, and it is
  counted in slices of the select the pipes are already read with, so no
  clock is read and a machine that steps its clock cannot shorten or extend
  the wait (`78f6909`)
- Add `Par.race`: the first thunk to finish wins. No worker limit, because
  the count is the length of the list and the list is at the call site. First
  to finish, not first to succeed — a loser that raises is discarded, and a
  winner that raises comes back as `Error`. Cancellation is cooperative and
  reuses the Ctrl-C machinery, so a loser doing wand work stops at its next
  step and every worker is joined before `race` returns. A loser waiting on a
  command waits for the command; `Shell.timeout` in the thunk is that bound
  (`ef7e2fa`)
- Add `Par.timeout`, a deadline on wand code, written in wand over `race` and
  `Clock.sleep`: the work and a sleeper race, and whichever finishes first
  answers. That makes it a wait of a length rather than a wait until an
  instant, which is what keeps it right on a machine whose clock steps. The
  work is asked to stop when the deadline passes and stops at its next step,
  giving back what it holds (`59e622a`)
- Order the temporal types. `<`, `>`, `<=` and `>=` now take an `Ord`, a type
  wand orders, and seven are ordered: Int, Float, String, Duration, Date,
  Time and DateTime. A comparison is on the value, not on the text, so
  `90s > 1min` is true; a `DateTime` normalizes to an instant, and a value
  with no offset is read as UTC, because reading it as local time would make
  one script answer differently on two machines. `Ord` composes as `Num`
  does, so `let later a b = if a < b then b else a` stays polymorphic
  (`0be8d0e`)

### Changed

- **Breaking:** Comparing a type wand does not order is a type error where it
  is written, not a runtime error during the run. `100MB < 1GB` typechecked
  before and failed mid-run; two functions could be compared at all. Size,
  Version, Port and IPv4 are not ordered yet —
  `docs/design/ordering-domain-types.md` decides what each one needs
  (`0be8d0e`)
- **Breaking:** Equality normalizes with ordering. `60s == 1min` was false
  while `60s < 1min` and `60s > 1min` were both false as well — three answers
  no reader can hold at once. It is now true. This breaks anyone who relied
  on `60s != 1min`, which is not a fact to rely on. `List.sort` follows the
  same rule where wand defines an order, and keeps structural comparison
  everywhere else (`0be8d0e`)
- Equality walks into a value. `60s == 1min` was true and `[60s] == [1min]`
  was false; the walk now goes through tuples, lists, constructors and maps
  (`f58c626`)
- `wand f` brackets a `handle` body that wraps. It produced source it could
  not read back: `with` has to follow the body, and a body that opened a
  bracket on one line and closed it on a later one left text for the newline
  to cut off (`f58c626`)

Reading the clock is not here, a virtual clock does not shorten a real
deadline, and a killed command may leave children.

## [0.24.0] - 2026-08-21

### Added

- Add a type on a parameter: `let describe (p: Pod) = p.name`, and
  `fn (p: Pod) -> ...` with it. This is what lets a function read a field
  off a parameter. Dot access needs a named type, and wand generalizes a
  definition before it sees any call, so the type has to come from the
  definition. The annotation works in each place a pattern does: a `let`,
  a `fn`, an arm of a `match`, and a `with ... as`. It composes with the
  return annotation: `let describe (p: Pod) : String = p.name`. The
  parentheses are part of the syntax, because a `:` between expressions is
  cons (`bec0356`)

`(x : xs)` still reports the cons message. Cons in a pattern is `[h : t]`,
in brackets, so the parenthesised form was never a pattern. One token tells
the two apart: a type starts with `Upper`, `'a` or `(`.

A type variable in a parameter is a type error. Each annotation resolves its
own names, so `'a` in two parameters would be two variables. Write the type
of the whole definition instead: `let f : 'a -> 'a = ...`.

## [0.23.0] - 2026-08-21

### Changed

- **Breaking (text):** A type error names what it expected and what it got.
  It said `cannot unify Glob with Path`, and it now says
  `expected Glob, got Path`. "Unify" is a word from the type checker, not
  from a script. Which type came from where was not recorded, so 37 places
  now state which side the reader wrote: an annotation, an application, an `if`,
  an arm of a `match`, a pattern, an element of a list or a map, a `$()`
  payload, a `|>` stage, a contract clause, a `with` resource, an operand.
  A site that cannot know says `Glob and Path are not the same type`
  (`f8437a2`)
- **Breaking (text):** An effect error names the difference. It said
  `cannot unify effects {Shell} with {Raise, Shell | ..}`, and it now says
  `the type allows {Shell}, but the body performs Raise`. At an argument it
  says `the parameter allows {}, but the function given performs Raise,
  Shell` (`f8437a2`)
- **Breaking (text):** Three more messages drop the word: a non-number in
  arithmetic reads `expected a number, got Bool`, the two members of `Num`
  read `Int and Float do not mix`, and a type that would contain itself
  reads `this value would have to contain itself` (`f8437a2`)
- `README.md` and `docs/reference.md` are written in Simplified Technical
  English: short sentences, active voice, one idea in each. The reference
  went from 692 sentences with a median of 14 words to 1158 with a median of
  9. Sentences over 20 words: 218 before, 42 now. No code block, heading or
  link changed (`782d8d5`, `c99c382`..`18e0614`)

### Fixed

- Fix `docs/reference.md` stating that a newline always ends a statement. A
  line that starts with an operator continues the line above, which is what
  a pipeline that leads with `|>` needs. So `let a = 1` and then `-2` gives
  `-1`. The parser is right; the reference was wrong (`eb28ae5`)
- Fix three claims in `README.md`: the by-hand install named v0.10.0, the
  example error text did not match a run, and startup said 1.6 times
  `bash -c :` where three runs give about 2 times (`782d8d5`)

## [0.22.0] - 2026-08-21

### Changed

- **Breaking:** `FS.glob` and `FS.glob_in` do not walk through a symlink.
  A link out of the tree added files that the directory does not hold. A
  link to a parent made the walk repeat until the path was too long. A
  symlink that matches is still an answer. wand still follows the base
  directory (`74283b9`)
- **Breaking:** `FS.write_file` creates a file with mode 0644. It used the
  channel default of 0666. `FS.create_file` and `FS.append` already used
  0644. Under `umask 0` the file was writable by all users (`74283b9`)
- **Breaking:** `FS.copy` gives a new file the mode of the source file. A
  copied script keeps its executable bit. A copy of a 0600 file stays
  private. A destination that exists keeps its own mode (`74283b9`)
- **Breaking:** wand checks the type variables in an annotation.
  `let f : 'a -> 'a = fn x -> x + 1` is now a type error: the body accepts
  only `Int`. `let g : 'a -> 'b = fn x -> x` is an error too: the body
  makes the two types the same. An annotation with no type variable does
  not change (`db0ce43`)
- `--dry-run` answers with a new random path for each temp file and temp
  directory. It answered with `/tmp/wand-dry-run-dir` each time, and all
  users can write that directory (`74283b9`)

### Fixed

- Fix `String.join`: it dropped a first element that is empty.
  `String.join "," ["", "b"]` gave `b` and now gives `,b` (`db0ce43`)
- Fix `String.words`: it split on one space, so extra spaces became empty
  words and a tab split nothing. `String.words "  a  b  "` gave 7 elements
  and now gives 2. Any run of whitespace splits (`db0ce43`)
- Fix `Path.with_extension`: an extension without a dot removed the
  extension. `Path.with_extension "md" /a/b.txt` gave `/a/bmd` and now
  gives `/a/b.md`. `""` removes the extension (`db0ce43`)

## [0.21.0] - 2026-08-21

A security and honesty release. Every item below is a case where wand said
one thing and did another — a manifest that did not bound what ran, a type
that did not report a raise, an exit code that did not match the finding.

### Changed

- **Breaking:** A handler now discharges an effect only when it covers
  every operation carrying that effect. Effects are many-to-one over
  operations — Shell carries four, FS.Write ten — and a handler used to
  discharge the whole effect for each case it had, so the operations it
  did not name went on reaching the default handler and running for real
  with the signature saying they could not. A partial handler now keeps
  the effect, and a case naming an operation that does not exist is an
  error with the nearest real name suggested (`8f67f02`)
- **Breaking:** A function kept in a constructor field keeps its effects.
  A field's effects are inferred from the value it is built with, and
  three places threw that away, so `type Action = Action (Unit -> String)`
  holding `fn () -> $(touch x)` typechecked under `uses {}` and ran the
  command when the match took it back out (`7c02e94`)
- **Breaking:** `Env.load!` declares `FS.Read`. The primitive performs a
  real `FS!read_file`, but the effects it declared were written by hand as
  `{Env, Raise}`, so a file whose whole manifest was `uses {Env}` could
  read any path on disk (`60b09f7`)
- **Breaking:** A `Shell(...)` manifest bounds what runs inside a subshell
  `(...)`, a substitution `$(...)` and a backtick span, wherever they
  appear, including inside double quotes. Each was read as opaque text on
  the reasoning that a subshell belongs to the named binary's shell — it
  does not, the same shell runs it — so `Shell(echo)` admitted
  `$(echo $(whoami))`. `$((...))` stays arithmetic and is not checked
  (`8cb3cf2`)
- **Breaking:** A pattern that can fail makes its binding raise, whether
  the fields are named or positional. The rule is that a constructor
  pattern cannot mismatch when the value has no other constructor to be;
  it was applied to named patterns by spelling instead of by type, so
  `let area (Circle (radius = r)) = r` over `Circle | Square` claimed to
  be total. `let` bindings record it too: `let Ok v = r in ...` performs
  Raise. The same rule drops a false positive the other way — a
  positional pattern over a single-constructor type no longer carries a
  risk its type cannot hold, so a name like `unbox!` now trips V-BANG2
  (`083b2cb`)
- **Breaking:** `wand s` refuses a test file whose assertions are
  discarded. A test block answers with one outcome, so sequencing
  assertions with `;` threw away every one but the last and the file
  reported a pass however the run went. V-DROP2 reports it (`59d1d1c`)
- **Breaking:** `wand t --strict --json` exits 1 on a violation. The JSON
  called the finding an error and the command then reported success, so a
  CI step reading the exit code was told the file was clean by the run
  that had just failed it (`ab349c0`)
- A command given input on `|>` keeps its own stderr, as `$()` has always
  done. It went down a pipe and was discarded, so
  `report |> $(mail ops@example.com)` swallowed the one thing that would
  have said why it failed (`be21c28`)
- A closed reader downstream — `wand report.wand | head -3` — ends the run
  at 141 after unwinding, rather than killing wand where it stands. SIGPIPE
  is ignored, so `with` brackets release before the run ends (`be21c28`)

### Added

- Add written effects to type annotations: `! {Shell}`, `! {Shell | 'e}`
  and `! 'e` all parse, on the innermost arrow of a multi-argument type as
  an
  inferred one carries them. They are checked rather than assumed, so an
  annotation cannot quietly narrow what a function does. This is what lets
  a declaration state that a field's effects are the caller's —
  `raises: ((Unit -> 'b ! 'e) -> TestOutcome ! 'e)` — which is what makes
  `t.raises (fn () -> $(cmd))` a type error in a file whose manifest is
  `uses {}` (`22380fb`)
- Add `--` as the end of wand's own arguments when running a script.
  `--dry-run` and `--trace` are wand's wherever they appear before it, so
  a script that takes a flag of the same name could not be given one: the
  run silently became a rehearsal and the script never saw the argument.
  `wand deploy.wand -- --dry-run` now runs for real and hands the flag on
  (`ab349c0`)

### Fixed

- Fix `$?()` and `|>` hanging forever on a command that writes more than a
  pipe buffer to stderr, or is fed more than one on stdin. The two pipes
  were drained one after the other, so the child blocked writing the pipe
  wand was not reading and never reached the end of the one wand was
  waiting on. All three streams now move together (`be21c28`)
- Fix `%{x}` written between quotes of your own not being quoted at all.
  Wrapping the value in single quotes quotes nothing inside `"..."`, so
  `$(echo "hi %{name}")` put the value into text the shell still read and
  a name holding `$(whoami)` ran it. The value is escaped for the quote it
  lands in now, and the author's word stays one word (`8cb3cf2`)
- Fix `$()` ending at a quoted `)`. `$(echo "a)b")` was cut in half and
  the rest of the line read as wand source (`8cb3cf2`)
- Fix `wand f` emitting source it cannot read back: three of the five
  string openers the lexer reacts to — `%!{`, `$!{` and `#{` — came back
  unescaped (`083b2cb`)

Effect sets are called effect sets throughout the compiler; "row" named
the encoding rather than the idea and is gone from comments and internal
names (`7a2523b`, `5695853`). No user-visible text changed.

## [0.20.1] - 2026-08-20

### Fixed

- Fix `wand f` emitting source that does not parse, at seven more sites:
  the `in` tail of a `let` and its value, a lambda's body, an operand of
  a binary operator, a branch of an `if`, a `match` scrutinee, a `with`
  resource, and a pipeline stage. Each is a place an expression that
  wrapped was written without the brackets that keep it whole. Two of
  them did something worse than fail: they changed what the code meant.
  A value could be reformatted into a different program, and a splice
  that wrapped inside `%{...}` dropped its argument, a newline there
  ending the string. Splices now render against a margin nothing reaches
  and come back on one line however long they are (`3db3700`)
- Fix a handler that declines to resume an operation performed inside a
  `with`'s release reaching the top level as
  `Fun.Finally_raised: Abandoned` instead of unwinding (`3db3700`)

### Added

- Add a margin sweep to the formatter's tests: all 69 corpus files are
  formatted at 20, 30, 40, 60 and 92 columns, and the result has to
  parse, and formatting it a second time has to change nothing. Every
  one of the parse bugs above needed a line long enough to wrap before
  it showed; a narrow margin makes every line long enough. It found all
  seven (`3db3700`)

The new guards are conservative — they bracket wherever a wrapped
application could be misread, and inside a bracket it could not — so ten
corpus files gain parentheses they do not strictly need.

## [0.20.0] - 2026-08-19

### Changed

- **Breaking:** `JSON.read_file`, `CSV.read_file` and `TOML.read_file`
  reached the disk through builtins of their own, declaring no effects
  at all — so a script read a file with nothing in its manifest saying
  so, a handler mocking the filesystem did not stand in for them, and
  `--dry-run` could not see them. All six now read through the same
  `FS!read_file` every other reader performs and parse the string with
  the parser each module already had, gaining `! {FS.Read}` and, for
  the `!` siblings, `! {FS.Read, Raise}`. A script that reads a config
  through any of them must now say `FS.Read`; `wand t --fix` writes the
  line. Error text for a missing file is now `FS.read_file`'s rather
  than each parser's (`507c134`)
- Change `wand f` to close a bracket that ran onto more lines on a line
  of its own, at the indent that opened it, rather than wherever the
  last line happened to end (`2cde06b`)
- Change `wand f` to stand a manifest and the leading block of plain
  imports off from what follows them, whether or not the source did,
  and to collapse several blank lines to one (`6fcae4c`)

### Added

- Add completion for effect operations: typing `FS!` in an editor lists
  all twenty, each with what a case binds and resumes with and the
  sentence a handler author wants — "handles the `FS.Write` effect of
  `FS.write_file` and `FS.write_file!`". Nothing was offered before, so
  what appeared came from the editor's own guess at words in the buffer
  (`6bb3f40`)
- Add an operations table as one definition of what a handler can catch.
  Which effect an operation belongs to, and the types it carries, are read
  from it, and it can be listed, which is what the editor needed.
  `test_operations.wand` proves every claim in it by running each performer
  under a handler for its operation (`6bb3f40`)

### Fixed

- Fix `wand f` emitting source that does not parse, at three sites: a
  wrapped application in a match case body, in a single-clause
  `let f x = ...`, and in a `let ... and ...` group. Each ends at its
  first line, so the argument below it read as something new. Bindings
  written as multiple equations have been guarded since the beginning;
  these paths never were (`a132dda`, `2cde06b`)
- Fix the format gate to cover `test/wand/` and `examples/` as well as
  `stdlib/` — 69 files against 22. Nothing was checking the other two,
  so seven fixtures had drifted. `tools/check_stdlib_fmt.wand` is
  `tools/check_fmt.wand` now (`a132dda`)
- Fix `wand f` measuring a list's and a tuple's items from the wrong
  column, so an item that broke internally wrapped to the left of the
  item itself (`2cde06b`)
- Fix the reference's table of interceptable operations, which had
  fallen four behind the binary. A drift test holds it there (`6bb3f40`)

`Shell!run_quiet` and `Shell!exit_code` have builtins behind them and
are answered by `--dry-run`, but nothing a script can write reaches
them. A handler case for either is legal and will never fire.

## [0.19.0] - 2026-08-19

### Added

- Add `Test.group`: child tests that share the group's label and
  whatever its body binds, nesting to any depth — a `Suite` is a third
  `TestOutcome` constructor, and `wand s` prints each child under the
  path of labels that led to it. Setup is the bindings above the child
  list, teardown a `with` bracket around it; a raise in the body is the
  group's one failure (`c0871aa`)

### Changed

- Change `Test.test` to catch a raise in its own block and return it as
  that test's labeled failure — `label: raised: <why>` — where it used
  to escape as an error carrying no label. A raising child no longer
  takes its siblings down with it (`c0871aa`)
- Change `wand f` to step in the continuation lines of an `if`,
  `match`, or `handle` that starts mid-line, rather than landing its
  `else` or its cases flush with the line that introduced it; an else-if
  chain stays one flat ladder (`ab28876`)
- Change `wand f` to open a list, map, tuple or sequence bracket on
  the line that introduces it — after `=`, after `in`, and after a
  trailing lambda's `->` — instead of giving the bracket a line of its
  own (`04c60f7`)

### Fixed

- Fix `wand f` measuring a map's width from the indent it wraps to
  rather than the column its brace lands at, which let a map run past
  the margin (`04c60f7`)
- Fix `wand f` rendering a list's and a tuple's items at the
  sequence's own indent while placing them two columns in, so an item
  that broke internally wrapped to the left of the item itself
  (`04c60f7`)
- Fix the unbound-name hints to name the command as the usage output
  lists it: an unknown name points at `wand v`, not `wand env`. The
  documentation follows throughout — every command is written in the
  short form that heads its usage line. Both spellings still dispatch
  (`2332623`)

## [0.18.1] - 2026-08-19

### Changed

- Change `$()`/`$?()` to exec a command directly when the shell would
  find nothing to do in it — no operators, expansions, quotes, or
  builtins — skipping `/bin/sh`'s startup per spawn (~5ms on macOS,
  whose /bin/sh is bash). Anything shell-shaped still runs through
  `/bin/sh`, and a missing program reports exit 127 with sh's stderr
  line on either path (`c678a8e`)

## [0.18.0] - 2026-08-19

### Removed

- Remove the bracket map forms: `[x = 1]` literals, `[x = a]` patterns,
  and `let [test] = import Test` no longer parse, each refused with an
  error naming the brace spelling — "a map is written in braces --
  {k = v}, not [k = v]". The `A-MAP1` finding and its migration machinery
  retire with the syntax (`ba63d5c`)

### Fixed

- Fix `<=` and `>=` to order strings, as `<` and `>` always did —
  `"a" <= "b"` typechecked and then failed at run time (`7359637`)

## [0.17.0] - 2026-08-18

### Changed

- Change map syntax to braces: `{x = 1, y = 2}` literals, `{}` as the
  empty map (sugar for `Map.empty`, which stays), and brace map patterns
  that pun — `{status}` binds the key's value to a variable of its own
  name, `{x = a}` renames, and a quoted key takes `= pat`. Import
  destructuring follows: `let {test} = import Test`. Map values print in
  braces, and `wand fmt` writes every bracket form back as braces
  (`253ae90`, `1cdb76d`, `5ab4922`, `c9b793d`)
- Change the formatter to write a single-constructor type whose
  constructor repeats the type's name as the shorthand the parser already
  reads: `type Container(name: String, ready: Bool)` — named fields only;
  positional payloads, differently named constructors, and
  multi-constructor types keep the long form (`8f2f4d5`)

### Added

- Add `A-MAP1`: a map still written in brackets is an advisory finding
  carrying the flagged line with its brackets flipped, so `wand t --fix`
  migrates a file finding by finding to a fixed point. The bracket forms
  parse for this release and are removed in the next (`90f40eb`)

## [0.16.0] - 2026-08-18

### Changed

- Change the formatter to indent a nested match as a block, the same shape
  a multi-line `(...)` sequence gets: the opening paren ends the arrow's
  line, the nested match sits two spaces deeper than the outer arm, and
  the closing paren returns to the outer arm's column — instead of
  printing the nested arms flush with the outer ones. Handler arms with a
  match body format the same way (`44e66fe`)
- Change the formatter to move a string that escapes its quotes between
  backticks, where a quote is a quote: `"say \"hi\""` becomes
  `` `say "hi"` ``, splices intact for interpolated strings. The quoted
  form is kept when backticks could not reproduce the value exactly and
  visibly: a backtick in the text, a literal `%{`, or control characters
  (`9624144`)
- The demo scripts are reformatted with the current formatter, picking up
  0.13.0's canonical manifests; the decode examples' inline JSON loses its
  backslashes to the backtick preference (`44e66fe`, `9624144`)

## [0.15.0] - 2026-08-18

### Added

- Add hover for local names: parameters, `let ... in` names, and pattern
  variables now answer with their inferred type, at the binder and at every
  use — including inside `%{...}` splices. Locals carry no positions, so the
  item enclosing the cursor stands in for lexical scope, the innermost
  binding winning when an item rebinds a name (`65319f5`)
- Add documentation to completion items: the expandable panel shows the
  signature as a name-over-type block with the doc string beneath — the
  inline detail stays, but the suggest widget truncates it, so the full
  answer is one chevron away (`65319f5`)
- Add `make install` in `editors/vscode/`: one command runs `npm install`,
  compiles, packages the `.vsix`, and side-loads it, finding the `code` CLI
  in the macOS app bundle when it is not on `PATH` (`cacba1e`)

### Changed

- Change hover to render the name on its own line with the type beneath it,
  so a long signature — a wide effect set especially — no longer wraps
  mid-type (`65319f5`)
- Change the extension to resolve the wand binary itself when `wand.path`
  is the bare default and `PATH` has none: install.sh's `~/.local/bin`,
  then Homebrew's prefixes — a Dock-launched VS Code carries no shell
  `PATH` (`cacba1e`)
- Change the Rehearse lens to appear on every file — on the first line when
  there is no manifest — and to rehearse against `/dev/null`, so a script
  that reads stdin reports on empty input instead of blocking (`cacba1e`)

## [0.14.0] - 2026-08-18

### Added

- Add `--json` to `wand d`: one object on stdout — `name`, `type`, `doc` — with a fact the session lacks reported as `null` rather than omitted, so "no doc" reads as an answer and not a schema difference (`6f12546`)
- Add `--json` to `wand v`: an array over the scope, bindings as `{"name","type"}` and modules as `{"name","module":true}`; `wand v --json <module>` lists the members with qualified names, so an entry feeds straight into a follow-up `wand d` or `wand t` (`6f12546`)
- Add `--json` to `wand s`: one object for the whole run — per-test entries under `tests` (a pass carries its `label`, a fail its `message`; a test that raised reports `"error"`, and both count as failed), files that would not load under `errors`, and the `passed`/`failed` counts. While the tests run their own prints go to stderr, so stdout holds nothing but the JSON; exit codes are unchanged (`6f12546`)

With these, every command whose output a tool might read — `t`, `d`, `v`, `s` — has a `--json` form. Each shape is documented in the reference's `--json` section.

## [0.13.1] - 2026-08-18

### Fixed

- Fix multi-line REPL entry for local `let` chains: a line that is itself an open binding — a bare `=` no `in` closes — now keeps the continuation prompt up until a line supplies the body (`in fib 10`, or a plain expression). Submitting there did not even error before: a chain's body may be implicit, so the prefix parsed, bound `Unit`, and the remaining equations landed in fresh entries (`a19ebd5`)
- Fix mutual recursion in the REPL: a trailing `and` holds the entry open, so a group is entered by ending its first line with the `and` and finishing with a blank line — the first line alone does not typecheck, its partner being unbound (`a19ebd5`)
- Fix a mutual group binding silently in the REPL: each name of the group is now echoed with its type, the way a lone binding is (`a19ebd5`)

### Changed

- Change the reference to state what the REPL actually does with repeated equations — specific patterns before catch-alls whatever the entry order, the newer of two equal patterns winning, a not-yet-exhaustive set accepted as `{Raise}` — and to show the mutual-recursion entry transcript and the real multi-line continuation rules (`a19ebd5`)

## [0.13.0] - 2026-08-18

### Changed

- Change effect labels to a single canonical order, alphabetical (`Env, FS.Read, FS.Write, IO, Proc, Raise, Shell`): displayed signatures, manifests, and suggestions all agree by construction (`9ff454e`)
- Change `wand fmt` to canonicalize the manifest — labels and `Shell(...)` binaries sorted, wrapping one per line past the column budget — and to alphabetize the leading block of plain imports; let-imports keep their order, and a comment in the region pins it (`9ff454e`)
- Change error reporting to carry positions as data end to end: lex errors now name their line and column (pointing at the start of the failing token), and manifest errors anchor at the manifest line — in both text and `--json` (`f06a293`)
- Change diagnostics to carry extents, not points: a finding spans the whole item it is about, a type error the whole expression at fault, and `--json` reports `end_line`/`end_col` when a diagnostic has real width (`a1f71e0`)

### Added

- Add `wand t --fix`: apply every machine-applicable correction to the file in place — the suggested manifest line, dead imports, corrected lines — re-checking to a fixed point; a parse error, or a type error carrying no fix, refuses the whole run and nothing is written (`2ee9e25`)
- Add `wand lsp`: the language server, a subcommand on the compiler binary so the editor can never disagree with `wand t`. Diagnostics as you type, hover showing the signature with its effects plus the doc string, completion (a member of an unimported stdlib module carries its `import` on accept), quick fixes from every finding that knows its correction, whole-document formatting, and go to definition — including into the standard library, opened from the binary as read-only documents (`162ebcd`, `ac3be7f`, `f527cc9`)
- Add editor auto-import: typing `FS.write_file!` in a buffer that has not imported `FS` inserts `import FS` into the sorted block and puts `FS.Write` into the manifest, with no gesture; `Shell` is never changed automatically — those edits stay one visible click away as quick fixes (`ac3be7f`)
- Add the VS Code extension at `editors/vscode/`: domain literals highlighted as the constants they are, embedded shell highlighting inside `$()`/`$?()` with `%{...}` splices back to wand, and a "Rehearse (dry run)" code lens on the `uses {...}` line; not on the Marketplace yet — its README shows how to build and sideload (`84936e1`)

## [0.12.0] - 2026-08-17

### Changed

- Change a destructuring `let` to bind a list pattern's leading elements and ignore the rest, as map patterns already do with keys; `match` arms and function equations still require the exact length (`f598bc3`)
- Change `wand h` and the REPL's `:h` to list commands short form first, alphabetized by it (`64f5f85`)
- Change `:reset` to clear the screen along with the session's bindings (`6a01dd9`)

### Added

- Add REPL shortcuts `:v` (`:env`), `:c` (`:clear`), `:s` (`:reset`) (`6a01dd9`)
- Add command shortcuts `v` (`env`), `f` (`fmt`), `s` (`test`), `V` (`version`) (`1b74e02`)
- Add `V-IMP1`: two imports in the leading import block binding the same name leave the first binding dead — rename one (`[parse = csv_parse]`) or drop it (`def6aa0`)

### Removed

- **Breaking:** remove `--version` and `-V`; the version is a command, `wand V` or `wand version` (`4db26e7`)
- **Breaking:** remove the `format` alias for `fmt` (`1b74e02`)
- **Breaking:** remove `:quit` and `:q`; `:x` (`:exit`) is the one way out of the REPL (`6a01dd9`)

## [0.11.0] - 2026-08-17

### Changed

- Change arithmetic (`+ - * /`, unary `-`) to be polymorphic over `Int` and `Float`; `Num` in a signature means either, decided at use (`77dbe38`)
- Change error messages to name the wand construct instead of a source language (`18b0131`)

### Added

- Add `Stream`: fold files of any size in bounded memory — `FS.stream_lines`, `IO.stdin_lines`, `Test.with_lines` (`539eeee`)
- Add `Float` module: `of_int`, `round`, `floor`, `ceil`, `abs` (`77dbe38`)
- Add corrective errors for `^`, OCaml float operators, character literals, `begin ... end`, and foreign stdlib names such as `List.iter` (`35379bf`)
- Add discovery pointers to unbound-name errors: `'wand env' lists the modules, 'wand env List' one module's members` (`35379bf`)
- Add `install.sh`: one-line install with platform detection and checksum verification (`a871d73`)

[unreleased]: https://github.com/wand-lang/wand/compare/v0.80.5...HEAD
[0.80.7]: https://github.com/wand-lang/wand/compare/v0.80.6...v0.80.7
[0.80.6]: https://github.com/wand-lang/wand/compare/v0.80.5...v0.80.6
[0.80.5]: https://github.com/wand-lang/wand/compare/v0.80.4...v0.80.5
[0.80.4]: https://github.com/wand-lang/wand/compare/v0.80.3...v0.80.4
[0.80.3]: https://github.com/wand-lang/wand/compare/v0.80.2...v0.80.3
[0.80.2]: https://github.com/wand-lang/wand/compare/v0.80.1...v0.80.2
[0.80.1]: https://github.com/wand-lang/wand/compare/v0.80.0...v0.80.1
[0.80.0]: https://github.com/wand-lang/wand/compare/v0.79.0...v0.80.0
[0.79.0]: https://github.com/wand-lang/wand/compare/v0.78.0...v0.79.0
[0.78.0]: https://github.com/wand-lang/wand/compare/v0.77.1...v0.78.0
[0.77.1]: https://github.com/wand-lang/wand/compare/v0.77.0...v0.77.1
[0.77.0]: https://github.com/wand-lang/wand/compare/v0.76.0...v0.77.0
[0.76.0]: https://github.com/wand-lang/wand/compare/v0.75.0...v0.76.0
[0.75.0]: https://github.com/wand-lang/wand/compare/v0.74.0...v0.75.0
[0.74.0]: https://github.com/wand-lang/wand/compare/v0.73.0...v0.74.0
[0.73.0]: https://github.com/wand-lang/wand/compare/v0.72.0...v0.73.0
[0.72.0]: https://github.com/wand-lang/wand/compare/v0.71.0...v0.72.0
[0.71.0]: https://github.com/wand-lang/wand/compare/v0.70.0...v0.71.0
[0.70.0]: https://github.com/wand-lang/wand/compare/v0.69.0...v0.70.0
[0.69.0]: https://github.com/wand-lang/wand/compare/v0.68.0...v0.69.0
[0.68.0]: https://github.com/wand-lang/wand/compare/v0.67.0...v0.68.0
[0.67.0]: https://github.com/wand-lang/wand/compare/v0.66.1...v0.67.0
[0.66.1]: https://github.com/wand-lang/wand/compare/v0.66.0...v0.66.1
[0.66.0]: https://github.com/wand-lang/wand/compare/v0.65.0...v0.66.0
[0.65.0]: https://github.com/wand-lang/wand/compare/v0.64.0...v0.65.0
[0.64.0]: https://github.com/wand-lang/wand/compare/v0.63.0...v0.64.0
[0.63.0]: https://github.com/wand-lang/wand/compare/v0.62.1...v0.63.0
[0.62.1]: https://github.com/wand-lang/wand/compare/v0.62.0...v0.62.1
[0.62.0]: https://github.com/wand-lang/wand/compare/v0.61.0...v0.62.0
[0.61.0]: https://github.com/wand-lang/wand/compare/v0.60.0...v0.61.0
[0.60.0]: https://github.com/wand-lang/wand/compare/v0.59.4...v0.60.0
[0.59.4]: https://github.com/wand-lang/wand/compare/v0.59.3...v0.59.4
[0.59.3]: https://github.com/wand-lang/wand/compare/v0.59.2...v0.59.3
[0.59.2]: https://github.com/wand-lang/wand/compare/v0.59.1...v0.59.2
[0.59.1]: https://github.com/wand-lang/wand/compare/v0.59.0...v0.59.1
[0.59.0]: https://github.com/wand-lang/wand/compare/v0.58.0...v0.59.0
[0.58.0]: https://github.com/wand-lang/wand/compare/v0.57.1...v0.58.0
[0.57.1]: https://github.com/wand-lang/wand/compare/v0.57.0...v0.57.1
[0.57.0]: https://github.com/wand-lang/wand/compare/v0.56.0...v0.57.0
[0.56.0]: https://github.com/wand-lang/wand/compare/v0.55.5...v0.56.0
[0.55.5]: https://github.com/wand-lang/wand/compare/v0.55.4...v0.55.5
[0.55.4]: https://github.com/wand-lang/wand/compare/v0.55.3...v0.55.4
[0.55.3]: https://github.com/wand-lang/wand/compare/v0.55.2...v0.55.3
[0.55.2]: https://github.com/wand-lang/wand/compare/v0.55.1...v0.55.2
[0.55.1]: https://github.com/wand-lang/wand/compare/v0.55.0...v0.55.1
[0.55.0]: https://github.com/wand-lang/wand/compare/v0.54.0...v0.55.0
[0.54.0]: https://github.com/wand-lang/wand/compare/v0.53.2...v0.54.0
[0.53.2]: https://github.com/wand-lang/wand/compare/v0.53.1...v0.53.2
[0.53.1]: https://github.com/wand-lang/wand/compare/v0.53.0...v0.53.1
[0.53.0]: https://github.com/wand-lang/wand/compare/v0.52.0...v0.53.0
[0.52.0]: https://github.com/wand-lang/wand/compare/v0.51.0...v0.52.0
[0.51.0]: https://github.com/wand-lang/wand/compare/v0.50.0...v0.51.0
[0.50.0]: https://github.com/wand-lang/wand/compare/v0.49.0...v0.50.0
[0.49.0]: https://github.com/wand-lang/wand/compare/v0.48.1...v0.49.0
[0.48.1]: https://github.com/wand-lang/wand/compare/v0.48.0...v0.48.1
[0.48.0]: https://github.com/wand-lang/wand/compare/v0.47.0...v0.48.0
[0.47.0]: https://github.com/wand-lang/wand/compare/v0.46.0...v0.47.0
[0.46.0]: https://github.com/wand-lang/wand/compare/v0.45.0...v0.46.0
[0.45.0]: https://github.com/wand-lang/wand/compare/v0.44.0...v0.45.0
[0.44.0]: https://github.com/wand-lang/wand/compare/v0.43.1...v0.44.0
[0.43.1]: https://github.com/wand-lang/wand/compare/v0.43.0...v0.43.1
[0.43.0]: https://github.com/wand-lang/wand/compare/v0.42.0...v0.43.0
[0.42.0]: https://github.com/wand-lang/wand/compare/v0.41.0...v0.42.0
[0.41.0]: https://github.com/wand-lang/wand/compare/v0.40.0...v0.41.0
[0.40.0]: https://github.com/wand-lang/wand/compare/v0.39.0...v0.40.0
[0.39.0]: https://github.com/wand-lang/wand/compare/v0.38.0...v0.39.0
[0.38.0]: https://github.com/wand-lang/wand/compare/v0.37.0...v0.38.0
[0.37.0]: https://github.com/wand-lang/wand/compare/v0.36.0...v0.37.0
[0.36.0]: https://github.com/wand-lang/wand/compare/v0.35.0...v0.36.0
[0.35.0]: https://github.com/wand-lang/wand/compare/v0.34.0...v0.35.0
[0.34.0]: https://github.com/wand-lang/wand/compare/v0.33.0...v0.34.0
[0.33.0]: https://github.com/wand-lang/wand/compare/v0.32.0...v0.33.0
[0.32.0]: https://github.com/wand-lang/wand/compare/v0.31.0...v0.32.0
[0.31.0]: https://github.com/wand-lang/wand/compare/v0.30.0...v0.31.0
[0.30.0]: https://github.com/wand-lang/wand/compare/v0.29.0...v0.30.0
[0.29.0]: https://github.com/wand-lang/wand/compare/v0.28.0...v0.29.0
[0.28.0]: https://github.com/wand-lang/wand/compare/v0.27.0...v0.28.0
[0.27.0]: https://github.com/wand-lang/wand/compare/v0.26.0...v0.27.0
[0.26.0]: https://github.com/wand-lang/wand/compare/v0.25.0...v0.26.0
[0.25.0]: https://github.com/wand-lang/wand/compare/v0.24.0...v0.25.0
[0.24.0]: https://github.com/wand-lang/wand/compare/v0.23.0...v0.24.0
[0.23.0]: https://github.com/wand-lang/wand/compare/v0.22.0...v0.23.0
[0.22.0]: https://github.com/wand-lang/wand/compare/v0.21.0...v0.22.0
[0.21.0]: https://github.com/wand-lang/wand/compare/v0.20.1...v0.21.0
[0.20.1]: https://github.com/wand-lang/wand/compare/v0.20.0...v0.20.1
[0.20.0]: https://github.com/wand-lang/wand/compare/v0.19.0...v0.20.0
[0.19.0]: https://github.com/wand-lang/wand/compare/v0.18.1...v0.19.0
[0.18.1]: https://github.com/wand-lang/wand/compare/v0.18.0...v0.18.1
[0.18.0]: https://github.com/wand-lang/wand/compare/v0.17.0...v0.18.0
[0.17.0]: https://github.com/wand-lang/wand/compare/v0.16.0...v0.17.0
[0.16.0]: https://github.com/wand-lang/wand/compare/v0.15.0...v0.16.0
[0.15.0]: https://github.com/wand-lang/wand/compare/v0.14.0...v0.15.0
[0.14.0]: https://github.com/wand-lang/wand/compare/v0.13.1...v0.14.0
[0.13.1]: https://github.com/wand-lang/wand/compare/v0.13.0...v0.13.1
[0.13.0]: https://github.com/wand-lang/wand/compare/v0.12.0...v0.13.0
[0.12.0]: https://github.com/wand-lang/wand/compare/v0.11.0...v0.12.0
[0.11.0]: https://github.com/wand-lang/wand/releases/tag/v0.11.0
