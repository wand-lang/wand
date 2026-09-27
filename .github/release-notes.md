## 0.85.0 - 2026-09-27

Packages: import a package by its URL, pin its version in one file, and let
the types decide each release's version.

### Added

- **Packages.** A directory with a `wand.pkg` is a package, named by its
  URL. Start one, add a dependency, and import it:

  ```sh
  wand p init github.com/you/tool
  wand p add github.com/mjstahl/json
  ```

  ```
  import github.com/mjstahl/json
  json.parse text
  ```

  `wand.pkg` is the one file a package needs. You edit the record at the
  top; `wand p` writes the sections below it:

  ```
  { package = github.com/you/tool
  , wand    = 0.85.0
  , require =
      [ { path = github.com/mjstahl/json, version = 1.4.0 }
      ]
  }

  -- DO NOT EDIT: sum, written by `wand p`
  https://github.com/mjstahl/json 1.4.0 sha256:61af4036…
  ```

  - `git` fetches a version from its tag (`v1.4.0`) into
    `~/.cache/wand/pkg`, with your git credentials. Fetching runs none of
    the package's code, and a run or `wand t` fetches a missing version
    by itself.
  - The sum section holds the hash of every version the build reads. A
    version that does not match is an error, never a fetch again.
  - Each dependency gets the lowest version that satisfies every package
    that requires it. Nothing is upgraded until you ask:
    `wand p upgrade`, or `wand p upgrade github.com/mjstahl/json@1.5.0`.
  - Two majors of one package can be used side by side. Before 1.0,
    each minor counts as a major.
    `wand p add github.com/mjstahl/json@2.1.0 --name json2`, then
    `import json2`. When their types meet, the error names both:
    ``expected a `Value` from https://github.com/mjstahl/json 2.1.0, got a
    `Value` from https://github.com/mjstahl/json 1.4.0``.
  - `local = ../json` on an entry uses your own copy in place of the
    cache.
  - `wand p tidy` makes `require` match the imports: it adds what they
    use and removes what they do not.
  - The `wand` field is the range of wand versions the package works
    with, and a wand outside it refuses to run the package.
  - A file or directory whose name starts with `_` is private to its
    package. Another package cannot import it.
  - In an import and in `wand.pkg`, a URL may leave out `https://`.

- **Releases the types decide.** `wand p interface` writes the package's
  public interface into `wand.pkg`, so a change to it shows in review:

  ```
  -- DO NOT EDIT: interface, written by `wand p`
  version 0.3.1

  type digest.Algorithm = Sha256 | Sha512

  digest.name : Algorithm -> String
  ```

  `wand p release` compares the interface with the last release and picks
  the version. A removed or changed export is a major release, an added
  one a minor release, and no change a patch. Before 1.0 a breaking
  change moves the minor number. A smaller release than the change needs
  is refused, with the lines that need the larger one. The command
  commits `wand.pkg` and tags `v<version>`. `wand p interface --check`
  fails in CI when the section is out of date.

### Changed

- **A bare import binds the file's name.**

  ```
  -- before
  let utils = import ./utils
  -- now
  import ./utils
  utils.double 5
  ```

  `.wand` is not part of the name. When the last segment is not a name,
  as in `./json-parser`, the error gives the `let` line to write.

- **One name, one import.** Two imports that bind one name, or an import
  and a top-level `let` of that name, are now an error at the second.
  Before, `V-IMP1` warned and the second one silently won.

  ```
  import List
  import ./List
  -- error: `List` is already bound by `import List` (standard library) on line 1.
  --        Rename this one: `let my_list = import ./List`
  ```

### Removed

- `V-IMP1`. The error above replaces it.
