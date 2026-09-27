## 0.87.0 - 2026-09-27

Run a script by its package URL, and a fix for types that modules name through their imports.

### Added

- **`wand <url>` runs a script from a package you require.** A module URL
  goes where a file goes:

  ```sh
  wand p add github.com/you/tool
  wand github.com/you/tool/cli gen
  ```

  The URL resolves as an import of it would: your `wand.pkg` must require
  it, and it gives the version. The copy is fetched and checked against
  the sum section. The script's manifest holds, and `--dry-run`, `--trace`,
  `--lint`, `--strict` and `--` work as they do for a file. The script's
  own imports resolve against your build, not against its `wand.pkg`.

  A URL that `wand.pkg` does not require is an error that names the
  `wand p add` to run. A file on disk always wins over a URL of the same
  name, so no command that worked before changes.

### Fixed

- **A type that a module names through its own import keeps that meaning
  in the file that imports the module.** Say `core.wand` declares a field
  with a type from another module:

  ```
  let M = import ./meta
  type Pod(metadata: M.Meta, image: String)
  ```

  A file that imported `meta` under another name, such as
  `let X = import ./meta`, got "unknown type 'M.Meta'". It worked only when
  both files used the same alias. The same was true for a bare `Meta` that
  `core.wand` got with `let {Meta} = import ./meta`. And a file that did not
  import `meta` at all could not use `Core.Pod.decoder`: "no decoder is
  known for type 'Meta'". All three work now.
