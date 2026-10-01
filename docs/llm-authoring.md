# Writing wand with an LLM

wand is built for work that a model writes and a person reads. This document
says what in the language serves that, and why each choice was made. It is
design rationale. It is not a benchmark.

- [Why ML-style syntax](#why-ml-style-syntax)
- [What the language does beyond syntax](#what-the-language-does-beyond-syntax)
- [Errors a machine can act on](#errors-a-machine-can-act-on)
- [How to check any of this](#how-to-check-any-of-this)

## Why ML-style syntax

Three syntax families were considered: ML, Algol and Wirth. ML won. A model
was asked which it wrote most reliably, and it said ML. That is weak
evidence. A model is not a reliable narrator about its own processing. Ask
the question twice and the answer can change.

The mechanical reasons are stronger, and they hold whoever is writing.

**Everything is an expression.** There is no statement form and no expression
form. So there is no choice to make before writing a line. Generation runs
left to right. A fragment can be correct before its surroundings exist. Algol
style forces the choice constantly, and a wrong choice means going back.

**There are no declaration blocks.** Wirth style puts `var`, `type` and
`const` sections above the body. A writer must know every local before
writing any of them. That is planning that is not local. A writer that works
forward is bad at it.

**Types are inferred.** A writer does not have to commit to an annotation it
might get wrong. Fewer early commitments means fewer contradictions to undo.
An annotation is still available where it helps a reader.

**Patterns follow the data.** A `match` takes its shape from the type. The
compiler reports a missing arm. A mistake becomes an error, not a silent
fallthrough.

**There are few ways to write the same thing.** No braces against
`begin`/`end`. No statement terminators. No semicolon rules. Fewer degrees of
freedom means fewer ways to be subtly wrong.

**Programs are shorter.** Fewer tokens is fewer chances to err. This alone
would move the number.

One point cuts the other way, and it matters. Algol-family code is far more
common than ML-family code. If volume of prior text decided this, C and Java
style would win easily. They did not. That is the reason to take the
structural argument seriously.

## What the language does beyond syntax

Syntax decides whether a program parses. These decide whether a wrong program
is caught.

**The first line bounds the blast radius.** A script declares what it
touches:

```ocaml
uses {FS.Write, Shell(git)}
```

The compiler compares the declaration with the code. Declare too little and
the file does not typecheck. A model writes faster than a person reads. The
manifest tells the reader what to check for, on line one.

**Values carry their type.** Paths, globs, durations, sizes, URLs, dates and
addresses each have their own literal and their own type:

```ocaml
import FS
let log_dir = /var/log
FS.glob log_dir           -- type error: expected Glob, got Path
```

A confusion between a path and a pattern is a compile error. In a language
where both are strings, it is a bug found in production.

**A name says its risk.** An operation that can fail returns a `Result`. Each
one has a `!` sibling that raises instead. The reviewer sees the risk at the
call site, in the diff, without opening the callee.

**A name means one thing.** `V-SHADOW1` reports a top-level name bound twice
in one file. A reader scanning for a name finds one binding.

**A module is named in upper case.** `Engine.step` reads as `List.map` does,
whether the module is the standard library's or the program's. A file named
`Engine.wand` is imported as `import ./Engine`; any other file is bound with
`let Engine = import ./engine`. `V-MOD1` reports a module bound in lower case.

**A hole asks the question.** Write the part you are sure of. Leave `?` for
the part you are not. Then ask:

```ocaml
let levels = Stream.fold_left ? Map.empty (IO.stdin_lines ())
```

```
$ wand t summarize.wand
Hole: Map 'a -> String -> Map 'a ! {IO, Raise | 'e}
```

The answer is the signature to write. This turns guessing into a lookup.

**A run can be rehearsed.** `wand t` checks and does not run. `--dry-run`
prints what a run would do: *would write*, *would delete*, *would run*. A
model can propose a script and a person can see its effects before it has
any.

## Errors a machine can act on

A diagnostic is data, not prose to be parsed:

```
$ wand t bad.wand --json
[{"severity":"error","code":"E-LEX","file":"bad.wand","line":1,"col":11,
  "end_line":1,"end_col":12,
  "message":"a comment is '-- ...' to the end of the line, not '//'",
  "fix":{"replace":{"from":"//","to":"--"}}}]
```

The `fix` is machine-applicable. `wand t --fix` applies it. A writer can
correct itself without reading the sentence.

Several messages exist only for habits carried in from another language:

| written | meant |
| --- | --- |
| `// ...` or `# ...` | `-- ...` |
| `${x}` or `#{x}` | `%{x}` |
| `"a" ^ "b"` | `"a" ++ "b"` |
| `let rec f` | `let f` |
| `and` or `or` as booleans | `&&` or `\|\|` |
| `not x` | `!x` |

`and` is not a mistake on its own. It joins mutually recursive bindings:
`let f ... and g ...`. The message says so, rather than only saying no.

Each names the correction. This table exists because a writer arriving with
habits from another language is the common case, not the exception. That
describes a model exactly.

## How to check any of this

The claims above are mechanism, not measurement. There is a way to measure
them.

`test/fuzz` mutates source that is already valid wand. That finds bugs, and
it cannot measure this: a mutant is not a program anyone meant, so nothing
about it says which syntax is easier to write.

The measurement needs a generator instead -- one that builds well-typed
programs from a type rather than from text, and that can print one AST in two
surface syntaxes. No such generator exists. With one: generate a large
sample, ask a model to reproduce each program from its description, and
compare how often each syntax parses on the first try.

That turns a model's self-report into a number. Until someone builds it and
runs it, treat this document as a record of reasoning, not a finding.
