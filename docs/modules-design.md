# Modules

The second of three records for package management; `imports-design.md`
lists all three. It needs a bare import to bind a name.

A file is a module, and the directory tree under a `wand.pkg` is a
package, named by its URL. A build uses the lowest version that satisfies every
requirement (Minimal Version Selection), so there is no resolver and no
surprise upgrade. There is no central registry, dependencies are verified
by hash, and fetching or installing one never runs its code.

## wand.pkg

`wand.pkg` sits at a package's root. It is a Wand record literal in a
data-only subset -- literals, records and lists, with no functions,
imports or effects -- so tools read and rewrite it without running code.

```
{ package = github.com/mjstahl/json
, wand    = 0.4.0
, require =
    [ { path = github.com/mjstahl/text, version = 1.2.0 }
    ]
}
```

- **Lexical types, not strings.** Module paths are `URL` literals and
  versions are `Version` literals, so a bad value fails when it is read.
- **Mostly tool-written.** The author writes the import line, and
  `wand p tidy` finds the module, picks the version and updates `require`,
  as `go mod tidy` does. Hand edits stay allowed for pinning a version or
  pointing at a local copy.
- **`wand` is a compatible range**, and the only language-version fact in
  the package: `0.4.0` accepts 0.4.0 up to 0.5.0, `1.2.0` accepts 1.2.0 up
  to 2.0.0. A build refuses a package whose range does not hold the
  running wand.
- **A local copy** is a `local` field on the entry. The build reads that
  directory instead of the cache, and the sum section does not check it:
  `{ path = github.com/mjstahl/json, version = 1.4.0, local = ../json }`.
- **The subset is fixed from the start**: never extended in a breaking
  way, so any wand reads any package's file. A JSON export of `wand.pkg` is
  deferred until something needs it.

A package is the directory tree under a `wand.pkg`. A file outside any
package imports only by path, as now.

## URL imports

```
import github.com/mjstahl/json          -- json.wand at the module's root
import github.com/mjstahl/json/decode   -- decode.wand in it
```

- The scheme is optional in an import and in `wand.pkg`, and a URL without
  one is `https`. Only there: elsewhere `github.com/x` is already field
  access and a path, so a URL keeps its scheme.
- The longest `require` path that is a prefix of the import names the
  module, and the rest of the URL names the file in it. The module's root
  file is named for the URL's last segment.
- An import line never carries a version: `wand.pkg` does.

## Visibility across packages

A leading `_` means private at every level. Names already work this way.
A module file (`_parser.wand`) or a directory (`_internal/`) with a `_`
segment is private to its package: importing it from another package is
an error. The interface section and this check use the same path test, so
they cannot disagree.

## Fetching

With `git`, which does the HTTPS and the user's credentials:

- `git ls-remote --tags <url>` lists the versions; a release is a tag
  (`v1.4.0`).
- A shallow clone of the tag fetches one, into the shared cache,
  `~/.cache/wand/pkg/<host>/<path>@<version>`, which is read-only once
  written.
- Fetching runs no code of the module's. Importing it later runs its
  bindings, as any import does; their effects are in the importer's types.

## One file

`wand.pkg` is the only file a package needs. The record comes first, and
people edit it. Below it, `wand p` writes two sections, the interface section
(see `release-design.md`) and then the sum section. Each opens with a
marker line, and the record ends at the first one:

```
-- DO NOT EDIT: interface, written by `wand p`
-- DO NOT EDIT: sum, written by `wand p`
```

Every marker starts with the same text, so `grep "^-- DO NOT EDIT"` finds
them all. A line that starts that way and is not one of the two markers
is an error, as is a section out of order or given twice.

## The sum section

Tool-written, one line per module version and its hash. A module read
from the cache is checked against it; a mismatch is an error, never a
refetch. `wand p tidy` adds the line for a version it fetches.

## Minimal Version Selection

The build reads the `require` lists of the whole graph and takes, for each
module, the highest version any of them requires -- the lowest that
satisfies them all. Nothing newer is chosen unless a `wand.pkg` says so.

## Major versions

Each major version of a dependency is a separate module in the build, so
majors coexist; before 1.0, each minor counts as a major. A json 1.x
`Value` and a json 2.x `Value` are different types.

A module that imports two majors directly names one with an alias, a
`require` entry with a `name`:

```
{ require =
    [ { path = github.com/mjstahl/json, version = 1.4.0 }
    , { name = json2, path = github.com/mjstahl/json, version = 2.1.0 }
    ]
}
```

```
import github.com/mjstahl/json   -- json, the 1.x entry
import json2                             -- the 2.x entry
```

Aliases are lowercase, so `import json2` is never read as a standard
library module.

**Type identity.** A type is keyed by its module and name already
(`canonical_type_name`). For a dependency the module key holds its URL
and major. When both sides of a type error have the same short name, the
message prints both qualified, with the exact version:

```
-- error: `v` is a `Value` from https://github.com/mjstahl/json 1.4.0,
--        but `validate` expects a `Value` from https://github.com/mjstahl/json 2.1.0.
```

## Commands

- `wand p init <url>` writes a `wand.pkg` naming the module, with the
  running wand's range and no requirements. It refuses a directory that
  has one.
- `wand p add <url>[@version] [--name <name>]` adds a `require` entry at
  that version, or the latest, and fetches what the build needs. It
  refuses a package already required at that major, and a second major
  with no `--name`; each refusal names the command that works.
- `wand p tidy` reads every import in the package: it adds a `require`
  entry, at the latest tagged version, for a module no entry names; drops
  entries nothing imports; fetches what is missing; and writes the sum
  section.
- `wand p upgrade` moves every direct dependency to its latest version
  within its major; `wand p upgrade <url>` moves one; `<url>@<version>`
  pins one. A new major is a different module, so moving to one is a
  change of import, never an upgrade.
- A script or `wand t` fetches a version `wand.pkg` names and the cache
  lacks, checked against the sum section. An import `wand.pkg` does not name is
  an error that says to run `wand p tidy`.

## Order

1. `wand.pkg`: the data-only reader, the `wand` range check, packages.
2. URL imports resolved through `require` and `local`, with subpaths and
   the private-path check.
3. Fetching with `git` into the cache, and the sum section.
4. Minimal Version Selection over the graph, and majors as modules with
   aliases and qualified type errors.
5. `wand p init`, `wand p tidy` and `wand p upgrade`, under a new `p`
   command group.
