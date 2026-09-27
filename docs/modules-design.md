# Modules

The second of three records for package management; `imports-design.md`
lists all three. It needs a bare import to bind a name.

A module is a URL. A build uses the lowest version that satisfies every
requirement (Minimal Version Selection), so there is no resolver and no
surprise upgrade. There is no central registry, dependencies are verified
by hash, and fetching or installing one never runs its code.

## wand.mod

`wand.mod` sits at a package's root. It is a Wand record literal in a
data-only subset -- literals, records and lists, with no functions,
imports or effects -- so tools read and rewrite it without running code.

```
{ module  = https://github.com/mjstahl/json
, wand    = 0.4.0
, require =
    [ { path = https://github.com/mjstahl/text, version = 1.2.0 }
    ]
}
```

- **Lexical types, not strings.** Module paths are `URL` literals and
  versions are `Version` literals, so a bad value fails when it is read.
- **Mostly tool-written.** The author writes the import line, and
  `wand tidy` finds the module, picks the version and updates `require`,
  as `go mod tidy` does. Hand edits stay allowed for pinning a version or
  pointing at a local copy.
- **`wand` is a compatible range**, and the only language-version fact in
  the package: `0.4.0` accepts 0.4.0 up to 0.5.0, `1.2.0` accepts 1.2.0 up
  to 2.0.0. A build refuses a package whose range does not hold the
  running wand.
- **A local copy** is a `local` field on the entry. The build reads that
  directory instead of the cache, and `wand.sum` does not check it:
  `{ path = https://github.com/mjstahl/json, version = 1.4.0, local = ../json }`.
- **The subset is fixed from the start**: never extended in a breaking
  way, so any wand reads any package's file. `wand mod --json` is
  deferred until something needs it.

A package is the directory tree under a `wand.mod`. A file outside any
package imports only by path, as now.

## URL imports

```
import https://github.com/mjstahl/json          -- json.wand at the module's root
import https://github.com/mjstahl/json/decode   -- decode.wand in it
```

- A URL always has its scheme; without one, `github.com/x` cannot be told
  from field access or division.
- The longest `require` path that is a prefix of the import names the
  module, and the rest of the URL names the file in it. The module's root
  file is named for the URL's last segment.
- An import line never carries a version: `wand.mod` does.

## Visibility across packages

A leading `_` means private at every level. Names already work this way.
A module file (`_parser.wand`) or a directory (`_internal/`) with a `_`
segment is private to its package: importing it from another package is
an error. `wand.api` generation and this check use the same path test, so
they cannot disagree.

## Fetching

With `git`, which does the HTTPS and the user's credentials:

- `git ls-remote --tags <url>` lists the versions; a release is a tag
  (`v1.4.0`).
- A shallow clone of the tag fetches one, into the shared cache,
  `~/.cache/wand/mod/<host>/<path>@<version>`, which is read-only once
  written.
- Fetching runs no code of the module's. Importing it later runs its
  bindings, as any import does; their effects are in the importer's types.

## wand.sum

Tool-written, one line per module version and its hash, beside `wand.mod`.
A module read from the cache is checked against it; a mismatch is an
error, never a refetch. `wand tidy` adds the line for a version it
fetches.

## Minimal Version Selection

The build reads the `require` lists of the whole graph and takes, for each
module, the highest version any of them requires -- the lowest that
satisfies them all. Nothing newer is chosen unless a `wand.mod` says so.

## Major versions

Each major version of a dependency is a separate module in the build, so
majors coexist; before 1.0, each minor counts as a major. A json 1.x
`Value` and a json 2.x `Value` are different types.

A module that imports two majors directly names one with an alias, a
`require` entry with a `name`:

```
{ require =
    [ { path = https://github.com/mjstahl/json, version = 1.4.0 }
    , { name = json2, path = https://github.com/mjstahl/json, version = 2.1.0 }
    ]
}
```

```
import https://github.com/mjstahl/json   -- json, the 1.x entry
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

## Questions

- The commands' short forms: `wand tidy`, and the one that fetches.

## Order

1. `wand.mod`: the data-only reader, the `wand` range check, packages.
2. URL imports resolved through `require` and `local`, with subpaths and
   the private-path check.
3. Fetching with `git` into the cache, and `wand.sum`.
4. Minimal Version Selection over the graph, and majors as modules with
   aliases and qualified type errors.
5. `wand tidy`.
