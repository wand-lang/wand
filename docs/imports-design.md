# Imports

The first of three records for package management. The design, and the
decisions its open questions were given, are in "Wand Package Management
Design" (claude.ai artifact DxkH3xhhDYxLZJaTP4Vx3R). The three records,
each released once:

1. `imports-design.md` (this record): an import binds a name of its own.
2. `modules-design.md`: `wand.mod`, URL imports, fetching with `git`,
   `wand.sum`, Minimal Version Selection and `wand tidy`.
3. `release-design.md`: `wand.api`, the version bump the types decide,
   and `wand release`.

Package management is built for predictable results over flexibility,
because an LLM is the main author.

The commands are one group, `p package`, each with its short form:

| Command | Does | Record |
|---|---|---|
| `wand p i` / `init <url>` | starts a package: writes `wand.mod` | modules |
| `wand p t` / `tidy` | makes `wand.mod` and `wand.sum` match the imports, fetching what they need | modules |
| `wand p u` / `upgrade [url[@version]]` | moves dependencies to newer versions within their major | modules |
| `wand p a` / `api [--check]` | writes `wand.api` from the code, or checks it | release |
| `wand p r` / `release [major\|minor\|patch]` | checks the bump, writes `wand.api`, tags | release |

`wand h p` lists them. A script or `wand t` that needs a version
`wand.mod` names and the cache lacks fetches it, checked against
`wand.sum`; an import `wand.mod` does not name is an error that says to
run `wand p tidy`. Only the package commands change `wand.mod`.

## An import binds the last segment of its path

```
import List                              -- stdlib, binds List
import ./utils                           -- binds utils
import ./lib/helpers.wand                -- binds helpers (.wand dropped)
let h = import ./lib/helpers             -- explicit name
let {parse, encode} = import ./json      -- selected names
```

- The last segment, without `.wand`, becomes the name if it is a valid
  identifier. Otherwise the `let` form is required, and the error says
  which: `./json-parser`, `../`.
- A private module needs no `let`: `import ./_internal` binds `_internal`.
- There are no glob imports. Names come from qualified access or an
  explicit destructure.
- A top-level import is not a member of the importing module: neither the
  module name it binds nor the names it destructures. This is how imports
  already behave.

Today a bare `import ./utils` is an error that asks for the `let` form.
That error, and the reference section that gives its reason, go.

## Two imports that bind one name

Two imports that bind the same name are an error at the second import
line. The message names both imports, says which one is the standard
library, and gives the `let` line that fixes it:

```
import List
import ./List
-- error: `List` is already bound by `import List` (standard library) on line 1.
--        Rename this one: `let my_list = import ./List`
```

The same holds for an import and a top-level `let` of the same name.

## Order

1. Bare path imports bind their last segment; the error for a segment
   that is not a name, and for two imports of one name.
2. The reference: the import section rewritten, and the style section's
   examples where they used the `let` form only to name a module.
