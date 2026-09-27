# Releasing a package

The third of three records for package management; `imports-design.md`
lists all three. It needs packages and `wand.pkg`.

The type system decides the minimum version bump. The author can raise it
but never lower it.

## The interface section

A snapshot of the package's public interface at its last release, in
`wand.pkg` after the record, in the `wand d --index` format, so no new
syntax is needed:

```
-- DO NOT EDIT: interface, written by `wand p`
version 0.3.1

type digest.Algorithm = Sha256 | Sha512 | Sha1 | Md5
type digest.Digest(algorithm: Algorithm, hex: String)

digest.of        : Algorithm -> String -> Digest
digest.read_file : Algorithm -> Path -> Result String Digest ! {FS.Read}
```

- A `version` line with a `Version` literal, the public type
  declarations, and every public member of every public module with its
  type, effects and interfaces. A type is named by its module, as a member
  is, because two modules may declare one name. Package-private modules
  and `test_` files are left out.
- Only `wand p release`, or `wand p interface`, writes it. `wand p interface`
  rewrites the interface and keeps the version line, so a pull request
  that changes the interface shows it as a diff here. Output is deterministic:
  modules and members are sorted, so one interface gives one text.
- `wand p interface --check` fails in CI when the file does not match the code, or
  its version does not match the latest tag.
- A change to the public interface shows as a diff in `wand.pkg`, for
  human and LLM reviewers.

It shares `wand.pkg` with the record people edit, behind a marker, so a
package has one file; `modules-design.md` gives the layout. It is
signatures rather than a record
because a record would hold types as strings, which need a second parse,
or as a syntax tree, which no one can review.

## The bump

The tool compares the public interface with the last release's interface section.

| Bump | Interface change |
|---|---|
| Major | An export is removed or renamed |
| Major | Any type of an export changes, a more general one included |
| Major | A variant is added to or removed from an exported sum type |
| Major | A field is added to or removed from an exported record |
| Major | A constraint is tightened, an effect is added, or an interface changes |
| Minor | A new export, type or public module is added |
| Patch | No change to the public interface |

Adding an export is always safe as a minor bump, because there are no
glob imports. Opaque types are not in this work, so every change to an
exported type's shape is major. A change of behaviour behind an unchanged
type is invisible to the types, so the author raises the bump by hand.

Before 1.0 the first release is 0.1.0, a breaking change bumps the minor
(0.3.0 to 0.4.0), and anything else bumps the patch.

## The command

```
wand p release [major|minor|patch]
```

The comparison is with the interface section at the last release's tag, so
interface changes made since, and shown in it by `wand p interface`,
all count.

- With no argument it uses the computed minimum.
- An argument lower than the minimum is refused, with the interface
  changes that need the higher bump.
- It refuses a working tree with changes, since the tag would not hold the
  code the interface section describes.
- On success it writes the interface section with the new version, commits it, and
  creates the git tag, in one step, so the tag and the file never
  disagree. It does not push.

## Left out

- A migration command that rewrites a package's code for a breaking
  wand and bumps its `wand` field, with `lib/fix.ml` and `lib/autoedit.ml`
  as its base. It gets a record of its own, when wand first makes a
  breaking change that code can be rewritten for.

## Order

1. `wand p interface`: generating the snapshot, and `--check`.
2. The comparison and the bump rules.
3. `wand p release`.
