# CLAUDE.md

**wand** — a typed ML-style scripting language built as a bash replacement,
designed for Human-AI collaboration. The implementation is OCaml, built with
Dune. `docs/reference.md` is the complete language reference; `README.md` is
the pitch.

Two kinds of work happen here. **Part A** is for changing the compiler
(OCaml). **Part B** is for writing `.wand` code — scripts, stdlib, tests,
examples. Most tasks need only one part.

---

## Part A — working on the compiler (OCaml)

### Layout

- `bin/wand.ml` — the CLI: dispatch for `t`/`i`/`d`/`v`/`f`/`s`, running a script by path or `-e` by expression, flags like `--dry-run` and `--trace`.
- `lib/` — the pipeline, one stage per module:
  - `token.ml`, `lexer.ml` — tokens and lexing, including domain literals (paths, globs, durations, sizes) and the string/command interpolation forms.
  - `parser.ml`, `ast.ml` — recursive-descent parser. A newline ends a statement unless the line below is indented past it, or opens with an operator; a bracket the statement opened suspends the rule until it closes. `stmt_col`/`stmt_depth` carry that anchor, and `clause_name` is what lets a function's next equation end the body above it.
  - `typechecker.ml`, `effect_set.ml` — Hindley-Milner inference extended with effect sets (the ten labels below); manifests are checked against inferred effects here.
  - `evaluator.ml` — tree-walking interpreter; effect handlers, `Par`, signals, shell execution.
  - `lint.ml`, `lint_rules.ml` — the `V-*`/`A-*` rules `wand t` reports.
  - `formatter.ml` — `wand f`; comments are never dropped or restyled.
  - `runner.ml` — the public API (`Runner.run_string`, `typecheck_file`, sessions); `repl.ml`; `compile_cache.ml`; `module_types.ml`; `util.ml`.
- `stdlib/*.wand` — the standard library, written in wand, embedded into the binary at build time by `tools/gen_stdlib_embed.ml`.
- `test/` — Alcotest suites (`test_*.ml`, one per area) plus `test/wand/*.wand`, which are wand-language tests run by `wand s`.
- `tools/check_fmt.wand` — CI gate that `stdlib/`, `test/wand/` and `examples/` are formatter fixed points. Run it locally as shown below.
- `tools/fuzz_sweep.wand` — runs the fuzzer over several seeds and keeps what each one finds. The fuzzer is OCaml; the orchestration around it is wand.
- `tools/check_port_imports.wand` — CI gate that every port `test/wand/test_ports.wand` imports keeps its effects behind an entry point. An import runs a file's bindings and not its bare expressions, so a port that puts work in a top-level binding runs it during the test suite, and the suite goes on passing.
- `tools/check_docs.wand` — CI gate that every stdlib function has a `>>` example and that every example produces what it says. The handful that cannot have one are listed in the script with the reason. `wand d -x <name>` prints a doc with its examples run; `wand d -t` checks them and says nothing when they hold.
- `test/fuzz/` — the fuzzer. `oracle.ml` holds the property (a typecheck of
  any input answers with a diagnostic; anything else is a finding),
  `mutate.ml` the edits it makes to the corpus, `fuzz.ml` the driver.
  `known.txt` lists signatures that are found and not yet fixed, so a
  scheduled run is red only for what is new; `regressions/` holds a reproducer
  for each one that is fixed, run by `test_fuzz_regressions.ml` on every PR.
  Run it locally as shown below.
- `.github/workflows/ci.yml` builds and tests on push/PR; `release.yml` builds release archives when a tag lands; `fuzz.yml` runs the fuzzer on four seeds twice a day and files an issue per new signature.

### Verifying a change

Check by exit code, never by reading output. `dune build @runtest 2>&1 | grep
-c "\[FAIL\]"` reports clean when nothing ran, and `set -e` does not abort a
`cmd; echo ok` sequence under zsh.

```bash
dune build                                            # exit code
dune build @runtest --force                           # exit code
dune exec test/test_parser.exe                        # one OCaml suite
_build/default/bin/wand.exe s test/wand               # the wand-level tests
for d in demos/0[1-8]-* demos/10-*; do $d/run.sh; done # each exit code
N=500 demos/09-fork-overhead/run.sh                   # ten seconds
WAND=$PWD/_build/default/bin/wand.exe \
  $PWD/_build/default/bin/wand.exe tools/check_fmt.wand
WAND=$PWD/_build/default/bin/wand.exe \
  $PWD/_build/default/bin/wand.exe tools/check_docs.wand
WAND=$PWD/_build/default/bin/wand.exe \
  $PWD/_build/default/bin/wand.exe tools/check_port_imports.wand
dune build @fmt                                       # dune files
make fuzz                                             # 20,000 inputs
make fuzz-eval                                        # and runs the programs
SEEDS=6 SECONDS_PER_SEED=300 make fuzz-sweep          # chasing a finding
```

`make fuzz` and `make fuzz-eval` take `SEED=N` and `ITERATIONS=N`, and exit
non-zero on a finding whose signature is not in `known.txt`, so either is
usable as a gate.

A fuzz run of 20,000 inputs takes about fifteen seconds and is worth doing
after a change to the lexer, the parser or the typechecker -- those three
are the whole of what it exercises, unless `--eval` is given. It exits 0 when
it finds nothing new.

`--eval` adds a third question, and `make fuzz-eval` is it: *run* the program
on both sides of a format and compare the answers. A formatting that keeps
the type and changes what the program does passes every other check here --
`p.M(N)(9 [])` came back as `p.M N (9 [])`, one argument where there were
two, and only a second pass disagreeing caught it. Two runs answer it
directly.

Only programs the typechecker says reach nothing outside themselves are run,
each in a process of its own with two seconds of its own. The gate is the
*inferred* effect set, never the `uses` line: a manifest bounds a file that
has one, and a file with none is unbounded rather than sealed. Gated on the
manifest, a single `delete-line` took the `uses` line off
`examples/ports/disk-threshold.wand` and the fuzzer ran `df`.

It costs a little over half the throughput -- 549 inputs a second becomes
241 -- so it covers less ground in the same time. `Fuzz` does not pass
it.

A finding is written to `_fuzz-findings/` as two files: the input, byte for
byte, and a `.json` beside it holding the seed, the iteration, the edits and
the backtrace. JSON because the scheduled job reads it to decide what to file.
`--seed S --only I` replays a finding, and `--input FILE --path P` rechecks
one. `tools/fuzz_sweep.wand` runs several seeds and keeps each seed's
findings in `_fuzz-findings/sweep/seedN/` -- a finding is keyed by signature,
so a second seed that hits one overwrites the first. The sweep clears
`sweep/` before it starts and nothing above it, so a finding left in
`_fuzz-findings/` by a plain run survives one. It pins the binary
before it starts, because a rebuild part way through means the findings
belong to no one build. When the bug is fixed, the input
moves to `test/fuzz/regressions/` with a `.txt` beside it saying what it
was, and `dune test` holds the fix in place from then on.

Changing `lib/formatter.ml` needs more: the formatter has produced source
that does not parse, so check that the corpus (stdlib, test/wand, examples,
demos and tools) is still a fixed point *and* still runs. `wand f` writes in
place, so run it on a copy.

Changing anything on the startup path gets before-and-after numbers in the
commit message, from several runs. Readings move ~15% between runs, so one
reading cannot tell an improvement from noise. There is no script for this;
time the two binaries directly.

The drift is slow as well as wide, so measuring one build and then the other
attributes to the change whatever the machine did in between. Keep both
binaries -- `cp _build/default/bin/wand.exe` before and after -- and
interleave the runs, taking the minimum rather than the median: drift only
ever adds. Sequential measurement of the same two builds has reported a 26%
cost that interleaving put at 4%.

Interleave in a *random* order, not a fixed one. A fixed rotation still puts
the same build last in every round, and whatever the machine drifts into
lands on that build every time. A fixed rotation reported the arithmetic
loop 5.8% slower across a release; randomising the order put the same two
builds within noise of each other, with the sign of the difference changing
between the minimum and the median.

Below about 3% there is nothing to read on this machine. Two builds that
differ only in where their code sits -- never-called functions added ahead
of the evaluator, or existing code moved across it -- measure 1-3% apart on
a tight interpreter loop while running identical instructions. A result
inside that band is layout, not work.

### Releasing

`VERSION` holds the number, and it is bumped in the commit that warrants it
rather than at release time — otherwise a build from `main` claims to be the
last release while behaving differently.

```bash
make release VERSION=0.6.0          # tags, pushes, builds the macOS x86_64
                                    # archive, attaches it
gh release edit v0.6.0 --draft=false
```

CI builds the other three archives when the tag lands. Both sides create the
release if it is missing, so they can finish in either order, and it stays a
draft until someone publishes it. `make release` refuses a tag that `VERSION`
disagrees with.

CI also signs a build attestation for each archive it builds, which is what
says who built one — the `.sha256` beside an archive is written by the same
job and says only that the download arrived whole. Check one with
`gh attestation verify <archive> --repo wand-lang/wand`. The macOS x86_64
archive is built here, so it has no attestation; `make release-archive` says
so when it finishes.

An attestation is bound to the repository that signed it, and moving the
repository does not carry it: 0.68.0 to 0.76.0 were signed under the earlier
name and verify under neither, since the old path is now an alias for the new
one rather than a place of its own. 0.77.0 was signed again from here after
the move, which rebuilt its three CI archives -- so their digests, and the
`.sha256` beside each, are not the ones the first build published.

The musl build is retried up to three times. It compiles dune from source,
and that has twice died with `Failed to allocate signal stack for domain 0` --
the OCaml runtime failing to start, before any of wand is reached. It does
not reproduce off the runners, so the retry is the fix. Three failures in a
row are not the flake; read the log.

---

## Part B — writing wand code

### The loop

Write the script, then let the tools drive the edits:

```bash
wand t script.wand          # typecheck a file (wand t -e "..." for a snippet)
wand t --fix script.wand    # apply the fixes findings carry (manifest lines, missing and dead imports)
wand f script.wand          # format in place
wand s                      # run every test_*.wand from here down
wand --dry-run script.wand  # report what it would change, without doing it
wand script.wand            # the real run
```

A rehearsal (`--dry-run`) withholds every change, reports it, and remembers
it: reads after a write answer from what the write would have put there, so
the script follows the path a real run would take. A withheld command
changes nothing a read can see.

Never write effect annotations by hand. `wand t` tells you the manifest line
to add, and `wand t --fix` applies it (with any other carried fixes) in
place. `--dry-run` comes before any real run of a script that writes or
deploys.

### Manifest and effects

The first line of a file that touches the world declares what it may do:

```
uses {Env, FS.Read, FS.Write, IO, Shell(curl, git)}
```

Those are five of the ten effect labels: `Shell` (subprocesses),
`FS.Read`, `FS.Write`, `Env`, `Net` (bytes to a host), `IO` (own streams),
`Proc` (exits), `Raise`, `Clock` (waits), `Random` (draws from entropy).
`Shell` covers naming a command as well as running one.
`Shell` may name the binaries the file runs — written as they are in
`$()`: `Shell(./probe.sh, docker-compose, git)` — and bare `Shell` means
any. `Net` narrows the same way, by host: `Net(api.github.com)` admits one
and bare `Net` any, and every redirect is held to the list too. wand has no
TLS of its own, so `HTTP` reaches a host through a `curl` subprocess, and a
narrowed `Shell` does not bound that one.
A subprocess is outside every label, `Shell`'s own list included:
`uses {Shell(curl)}` reaches any host with no `Net`, and `uses {Shell(cat)}`
reads any file with no `FS.Read`. The labels describe what a file's wand
code does, and wand reads the file rather than the binary. Never write a
manifest as though it bounded a subprocess.
A literal command word the list omits is a type error; a word decided
at run time is checked at spawn and flagged by `V-SHELL1`. Doing more than
the manifest says is a type error; declaring more than the file does is an
`A-USES1` warning. Effects are inferred — never annotated: `wand t`
suggests the exact manifest line, narrowed when it can read every command
word.

### Syntax card

The forms below are wand's; the parenthetical is the drift to avoid.

| wand | not |
|---|---|
| `-- comment`, and a run of them above a definition is its doc | `//`, `#`, `(* block *)` |
| `fn x -> x + 1` | `fun`, `\x ->`, `lambda` |
| `h :: t` cons, `[h :: t]` in patterns | `:` for cons, `(x : xs)` |
| `"hi %{name}"` interpolation | `${x}`, `#{x}`, f-strings |
| `&&`, `\|\|`, `!` | `and`, `or`, `not` |
| `let f n = ...` (already recursive) | `let rec` |
| `{a = 1}` map; `{a, b = x}` pattern (puns) | `{a: 1}`, `[a = 1]` |
| `T(r, b = 3)` record update | `{r with b = 3}` |
| `Pod(name, restarts)` fields, punned in a pattern or a construction | `Pod{name}`, positional fields |
| `Foo.Status`, `Foo.Live`; or `let {Status, Live} = import ./foo` | a bare imported type or constructor |
| `type Pod(name: String, tries: Int = 3)` field default | a second constructor, an `Option` for "not given" |
| `type Shape = Circle Int \| Rect Int Int` | `Circle of Int` |
| `try e` yields a `Result` | `try ... with`, `raise` |
| no mutation — bind a new name | `ref`, `mutable`, `:=` |

`type X = <a type>` is an alias — another name for a type that already
exists, not a new one: `type Point = (Int, Int)`, `type Ids = List Int`,
`type F = Int -> Int`, `type This = That`, and parameterised,
`type Pair 'a = ('a, 'a)`. It is transparent, so the two are one type and
interchangeable both ways; it buys a name, not a type the checker keeps
apart, which is what a record is for. It names whatever its target names, so
an alias to a single-constructor type builds and matches
(`type MyConf = Foo.Conf` gives `MyConf(port = :80)`); an alias to a
multi-constructor one is not a value. A type shows with the alias it was
written as — `Pair Int (= (Int, Int))`.

A name declares one thing. Two `type`s of one name, two constructors
sharing one, a value taking the name of a type or a constructor, or a
declaration over a built-in's name are all errors naming which to rename.

Arithmetic (`+ - * /`) works on `Int` and `Float` alike — one numeric
type per expression, never mixed implicitly (`Float.of_int` /
`Float.round` convert); `%` is `Int`-only; `'a: Num` in a signature is a
variable carrying a constraint -- "`Int` or `Float`, decided at use". `+` and `-` also add two `Size`s or
two `Duration`s (`Add` in a signature); `*` and `/` do not. A `Duration`
also moves a `DateTime`, and two `DateTime`s subtract to the `Duration`
between them — two instants do not add.

Statements: one per line at the top level, nothing else needed. A `;` after
a binding ends its right-hand side and hands the rest to its body, and needs
no brackets to do it:

```
let of_hex algorithm text =
  let want = _hex_length algorithm;
  let lower = String.lower text;
  String.length lower == want
```

A statement that binds nothing is joined to what follows by neither a `;`
nor a newline, so sequencing those still wants the parentheses:

```
let deploy! target = (
  FS.mkdir! (Path.of_string target);
  FS.write_file! (Path.of_string "%{target}/v") "1\n";
  "deployed"
)
```

`in`, the block's `;` and the newline that ends a right-hand side all bind
the name over the same body, so they are one thing and `wand f` writes the
`;`. The separator is the formatter's business rather than yours. Two
spellings survive it, and both say what the `;` cannot: `in` where the value
ends on a `match` or `handle` arm, since an arm reads a `;` on it as part of
itself, and `in` where it narrows — `(let x = 1 in x + 1; 9)` gives `x` to
`x + 1` and to nothing after it. Prefer one `match` over multi-equation
definitions. Pattern matching must be exhaustive or it is a type error.

### Domain literals

Values are written directly; the shape carries the type:

| literal | type |
|---|---|
| `/etc/hosts`, `./build`, `~/x` | Path |
| `*.wand`, `./file*.txt` | Glob — a *relative* glob with a directory part needs the `./` prefix |
| `30s`, `5min`, `2h` | Duration |
| `100MB`, `4KB` | Size |
| `2024-01-15T14:30:00Z`, `2024-01-15` | DateTime — the bare day is that day at midnight UTC |
| `https://example.com` | URL |
| `192.168.1.1`, `10.0.0.0/8` | IPv4, CIDR |
| `:8080` | Port |
| `1.2.3` | Version |
| `r/pat/i` | Regex |

There is one type for a point in time and one resolution, the second.
`14:30:00` is not a value — a time of day belongs to a day, so it is a
`Duration` on top of one: `DateTime.on! 2026 8 22 + 14h + 30min`. A value
prints in full and in UTC (`"%{2026-08-22}"` is `2026-08-22T00:00:00Z`);
`DateTime.date_string` writes the short form. Source keeps whichever
spelling was written.

`DateTime` opens an instant — `year`, `month`, `day`, `hour`, `minute`,
`second`, `weekday` (ISO 8601, Monday 1), `day_start`, `on`/`on!`,
`date_string`, `time_string`. It reads no clock: `Clock.now` does that, so
today at midnight is `Clock.now () |> DateTime.day_start`.

### Shell

- `$(git status)` runs a command, raises on failure, yields the trimmed stdout `String`.
- `$?(cmd)` yields a `ShellResult` for inspecting exit code/stderr instead of raising.
- `$*(cmd)` is the command itself, unrun: a `Command`, which `Shell.run!`, `Shell.run` and `Shell.query` take. `$()` and `$?()` are the first and third of those over one. Use it for a command that is named, passed or listed; nothing turns a `String` into one. Building one performs `Shell`, so the manifest answers for the words wherever they are run.
- Inside a command, `%{x}` splices a value quoted as one argument — use it for data. `%!{x}` splices text to be read *as shell source* — only for text that is deliberately shell (a flag string, a pipeline fragment).
- `report |> $?(mail ops@example.com)` pipes the left value to the command's stdin.
- `Shell.stream cmd` reads a command's output line by line — the answer for `tail -f` and friends, which `$()` cannot bound. Stopping the read kills the command; an early stop ignores its exit code.
- Parse captures with `Shell.lines`, `Shell.decode` — not by hand.

### Names and errors

- Fallible operations return `Result`/`Option`; every one has a raising sibling named with `!` (`FS.copy` returns a `Result`, `FS.copy!` raises). A function you write that can raise gets a `!` name too; predicates end in `?` (`failed?`).
- Handle a `Result` by matching it, or `try f x` to capture a raise as a `Result`. Discarding a `Result` silently is a `V-DROP1` warning; bind it to `_` only when the failure genuinely does not matter.
- A typed hole `?` asks the typechecker what belongs there: `List.fold_left ? Map.empty xs`, then `wand t` reports the hole's type.

### Imports and tests

```
import FS                       -- a stdlib module
let {test} = import Test        -- destructure specific names
let {helper} = import ./util    -- another file, by path
```

An import runs a file's bindings and not its statements, so importing a
script runs none of its work -- and a `let` at the top level whose value
reaches the world does run. That is what lets `test/wand/test_ports.wand`
test `examples/ports/` without running any of them.

Stdlib modules: List, String, Regex, Map, FS, Resource, Stream, Path, IO,
Float, Int, Proc, Env, CSV, JSON, TOML, YAML, Duration, Size, Clock,
DateTime, Par, Shell, Decode, Args, Test, Option, Result, Hash, Digest,
Base64, HTTP, URL, Glob, IPv4, CIDR, Port, Version, Random. Every function
comes from a module: printing is
`IO.println`, and a file that prints writes `import IO`.

Big files stream instead of loading (and `Shell.stream` reads a command the
same way): `FS.stream_lines log |>
Stream.filter p |> Stream.fold_left f init` — a stream reads nothing
until the fold runs it (folding again re-reads), and a missing file
raises at the fold, caught with `try`. `FS.stream_lines_all` reads several
files as one; `FS.write_lines!` and `FS.append_lines!` are the write end,
and open the file once.

Tests are wand files named `test_*.wand`:

```
let {test, group} = import Test
test "it adds" (fn t -> t.eq 3 (1 + 2))
test "it raises" (fn t -> t.raises (fn () -> List.head! []))
group "shared" (fn () -> let n = 6 * 7 in [test "n" (fn t -> t.eq 42 n)])
```

`group` children share the body's bindings and print under the group's
label (`shared / n`); groups nest. Setup is the code above the child
list, teardown a `with` bracket around it — no other lifecycle exists.

Cleanup uses `with r as x -> body` (released however the body ends);
parallelism is `Par.map`; JSON decoding derives from type definitions
(see the reference's Decoders section).

For anything not covered here, read `docs/reference.md` — including its
"Style for scripts" section, which names the dialect `examples/` and
`demos/` are written in.
