# Reading a command line

Six ways to read argv with the `Args` module, from the shortest to the most
particular. Every example here is a whole script, and every transcript is
what it actually prints.

A command line is an untyped boundary, so it is read the way wand reads any
other: argv becomes a document and a decoder reads it. There are no
combinators of its own — every one of `Decode`'s already applies, including
the domain readers and the error that names the field that went wrong.

- [1. A type is its decoder](#1-a-type-is-its-decoder)
- [2. On/off flags](#2-onoff-flags)
- [3. Defaults, and a flag that does not match its field](#3-defaults-and-a-flag-that-does-not-match-its-field)
- [4. Positional arguments](#4-positional-arguments)
- [5. When one flag decides the rest](#5-when-one-flag-decides-the-rest)
- [6. Flags and positionals together](#6-flags-and-positionals-together)
- [What to watch for](#what-to-watch-for)

---

## 1. A type is its decoder

The usual case, and the shortest. Field names are flag names, and the field
types do the reading — `Port`, `Duration` and `Path` are read from the
strings on the command line exactly as they would be from a config file.

```
uses {IO}

import Args
import IO
import Proc

type Opts(port: Port, timeout: Duration, config: Path)

match Args.parse Opts.decoder (Proc.args ()) with
| Ok o -> IO.println "port=%{o.port} timeout=%{o.timeout} config=%{o.config}"
| Error e -> IO.println_err "usage: %{e}"
```

```
$ serve.wand --port 8080 --timeout 30s --config ./app.toml
port=:8080 timeout=30s config=./app.toml

$ serve.wand --port=9090 --timeout=5min --config=/etc/x.toml
port=:9090 timeout=5min config=/etc/x.toml

$ serve.wand --port http --timeout 30s --config ./a.toml
usage: .port: expected Port, got "http"
```

`--port 8080` and `--port=8080` are the same thing. With `=`, only the first
one splits, so a value is free to contain more of them.

## 2. On/off flags

Every flag is assumed to take a value, because whether it does is the one
fact a list of strings cannot reveal: without being told, `--message -5` and
a flag followed by a positional argument are the same shape. The type says
which ones do not: a `Bool` field is a switch, and `Opts.parser` carries
that to `Args.read`.

An absent switch reads as `false`.

```
uses {IO}

import Args
import IO
import Proc

type Opts(verbose: Bool = false, force: Bool = false, target: String)

match Args.read Opts.parser (Proc.args ()) with
| Ok o -> IO.println "target=%{o.target} verbose=%{o.verbose} force=%{o.force}"
| Error e -> IO.println_err "usage: %{e}"
```

```
$ deploy.wand --target prod --verbose --force
target=prod verbose=true force=true

$ deploy.wand --target prod
target=prod verbose=false force=false

$ deploy.wand --force --target prod
target=prod verbose=false force=true
```

A flag that takes a value and is given nothing is an error, named:
`--config expects a value`.

## 3. Defaults, and a flag that does not match its field

A decoder is an ordinary value, so one can be built by hand where the derived
shape is not what is wanted. Here every flag is optional with a fallback, and
`--listen` fills a field called `port`: the flag name lives in the decoder,
not in the type.

```
uses {IO}

import Args
import Decode
import IO
import Option
import Proc

type Server(host: String, port: Port)

let or_else fallback name dec =
  Decode.map (fn v -> Option.default fallback v) (Decode.optional name dec)

let server = (Decode.map2
  (fn h p -> Server(host = h, port = p))
  (or_else "localhost" "host" Decode.string)
  (or_else :8080 "listen" Decode.port))

match Args.parse server (Proc.args ()) with
| Ok s -> IO.println "serving on %{s.host}%{s.port}"
| Error e -> IO.println_err "usage: %{e}"
```

```
$ server.wand
serving on localhost:8080

$ server.wand --host 0.0.0.0 --listen 9000
serving on 0.0.0.0:9000

$ server.wand --listen nope
usage: .listen: expected Port, got "nope"
```

A default does not cost the error message: a flag that is given a value it
cannot read still fails, and still says which flag.

`Decode.map2` is the widest of its kind. Past two fields, chain `and_then` as
in example 5, or read the flags into one type with a derived decoder and join
it to the rest as in example 6.

## 4. Positional arguments

Positional arguments arrive under `_`, which no flag can collide with: `--_`
is not a name anyone writes, and `_` is already the language's word for the
part with no name.

```
uses {IO}

import Args
import Decode
import IO
import List
import Proc

let files = Decode.field "_" (Decode.list Decode.path)

match Args.parse files (Proc.args ()) with
| Ok ps -> IO.println "%{List.length ps} file(s): %{ps}"
| Error e -> IO.println_err "usage: %{e}"
```

```
$ count.wand a.txt b.txt ./c.md
3 file(s): [a.txt, b.txt, ./c.md]

$ count.wand a.txt --out /tmp/o b.txt
2 file(s): [a.txt, b.txt]

$ count.wand
0 file(s): []

$ count.wand - -5
2 file(s): [-, -5]
```

The last line is the point of the rule that only `--name` is a flag: a single
dash is not one, which is what keeps `-5` an argument rather than an option
nobody declared. Short flags do not exist.

## 5. When one flag decides the rest

`Decode.and_then` reads one field and chooses what to read next — a mode
whose options differ per mode, refused by name when it is neither.

```
uses {IO}

import Args
import Decode
import IO
import Proc

type Cmd = Serve Port | Fetch URL

let cmd = (Decode.and_then
  (fn mode -> match mode with
  | "serve" -> Decode.map (fn p -> Serve p) (Decode.field "port" Decode.port)
  | "fetch" -> Decode.map (fn u -> Fetch u) (Decode.field "from" Decode.url)
  | other -> Decode.fail "unknown mode %{other}, expected serve or fetch")
  (Decode.field "mode" Decode.string))

match Args.parse cmd (Proc.args ()) with
| Ok (Serve p) -> IO.println "serving on %{p}"
| Ok (Fetch u) -> IO.println "fetching %{u}"
| Error e -> IO.println_err "usage: %{e}"
```

```
$ run.wand --mode serve --port 8080
serving on :8080

$ run.wand --mode fetch --from https://example.com
fetching https://example.com

$ run.wand --mode wat --port 1
usage: unknown mode wat, expected serve or fetch

$ run.wand --mode serve
usage: .port: no such field
```

The last two are the pair worth having: a mode nobody offers is refused with
the modes that are, and the right mode missing its own option is refused by
the name of what is missing.

## 6. Flags and positionals together

`_` is not a name a type can declare as a field, so a script that wants both
composes: the derived decoder reads the flags, and `map2` joins it to the
positional list.

```
uses {IO}

import Args
import Decode
import IO
import List
import Option
import Proc

type Flags(out: Path, timeout: Duration, rehearse: Option Bool)

let job = (Decode.map2
  (fn f ps -> (f, ps))
  Flags.decoder
  (Decode.field "_" (Decode.list Decode.path)))

let parser = Args.Parser(spec = {rehearse = "switch"}, reader = job, usage = "")

match Args.read parser (Proc.args ()) with
| Ok (f, ps) ->
  let mode = if Option.default false f.rehearse then "rehearsing" else "running" in
  IO.println "%{mode} %{List.length ps} file(s) -> %{f.out} within %{f.timeout}"
| Error e -> IO.println_err "usage: %{e}"
```

```
$ build.wand a.txt --out /tmp/build b.txt --timeout 2min c.txt
running 3 file(s) -> /tmp/build within 2min

$ build.wand --rehearse --out /tmp/b --timeout 30s a.txt
rehearsing 1 file(s) -> /tmp/b within 30s

$ build.wand a.txt --out /tmp/b
usage: .timeout: no such field
```

Flags and positionals interleave freely, as the first line shows: a flag and
its value are taken out wherever they sit, and what is left is positional.

## What to watch for

**`--dry-run` and `--trace` are wand's, and a script never sees them.**
They are taken wherever they appear, before the file or after it:

```
$ wand --dry-run deploy.wand   # rehearses
$ wand deploy.wand --dry-run   # rehearses too -- the script is handed nothing
```

That costs a script the use of those two names, deliberately. The
alternative is that the second line runs the deploy for real, and someone
reaching for a rehearsal and getting a deployment is the worse failure. A
script that wants a rehearsal flag of its own should call it something
else — `--rehearse`, `--plan`, `--check`.

Every other flag wand has — `--json`, `--expr`, `--load`, `--strict`,
`--fix` — belongs to a subcommand (`wand t`, `wand d`, `wand s`),
and no subcommand runs a script with arguments, so a script receives those
as ordinary words:

```
$ wand argv.wand --json --expr x
[--json, --expr, x]
```

**A derived decoder looks for the field's own name.** A field called
`dry_run` is filled by `--dry_run`, not by `--dry-run`. The mismatch is not
an error — the field is simply absent, and whatever default is in place
wins, quietly. Where a hyphenated spelling is wanted, read it by hand:

```
let rehearsing =
  Decode.map (fn v -> Option.default false v) (Decode.optional "dry-run" Decode.bool)
```

**`Proc.args ()` is the arguments alone.** There is no program name at the
front to skip, as there would be with bash's `$0` or C's `argv[0]`.
