## 0.86.0 - 2026-09-27

Packages from a clone, hyphenated names, and a `wand` field that newer wands accept.

### Added

- **`wand p init` names the package from the repository.** In a clone,
  the URL is optional: `origin` gives it.

  ```sh
  # before
  wand p init github.com/you/tool
  # now, in a clone of git@github.com:you/tool.git
  wand p init
  ```

  Outside a repository, below its top, or with no `origin`, it still asks
  for the URL.

- **Examples for services and packages.** Seven new ports in
  `examples/ports/`: a health server beside a refresh job, a TCP line
  server, a mirror check with deadlines, a batch with a progress line, a
  log alert, a long-running `bc`, and a count of build warnings.
  `examples/packages/` has a library and an app that requires it.

### Changed

- **A hyphen in an import becomes `_`.** A package or a file named with a
  hyphen imports without a `let`:

  ```
  -- before
  let pkg_fixture = import github.com/wand-lang/pkg-fixture
  -- now
  import github.com/wand-lang/pkg-fixture
  pkg_fixture.greet "you"
  ```

  The repository and its files keep the hyphen. A segment that is still
  not a name, such as `2fast`, needs the `let` form.

- **The `wand` field in `wand.pkg` is the oldest wand the package works
  with.** A newer wand runs the package, up to the next major. Before 1.0
  that is 1.0.0. Before, `wand = 0.85.0` refused 0.86.0, so every wand
  release refused every published package until its author released it
  again.

### Fixed

- **`wand p add` for a package you already require.** Say `wand.pkg`
  requires `github.com/wand-lang/pkg-fixture` at 0.1.0, and 0.2.0 is out:

  ```sh
  wand p add github.com/wand-lang/pkg-fixture/words
  # before: "... is already required at another major. Give this one a name"
  # now:    "... is already required at 0.1.0, and every file in it can be
  #          imported. To move it, run `wand p upgrade ...@<version>`"
  ```

  Before, `add` looked up the newest release, found 0.2.0, and told you to
  add it as a second package. Now it tells you the package is already
  there, so the import works as it is.
- A name that wand suggests for a second major is now always a valid
  name: `pkg_fixture0_2`, not `pkg-fixture0_2`.
