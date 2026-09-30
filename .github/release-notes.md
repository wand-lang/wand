## 0.94.0 - 2026-09-29

`wand p release` names the kind of change, and can release 1.0.0. `wand p upgrade`, `add` and `tidy` take only a version that your wand can use.

### Added

- **Release a version by its number.** `wand p release 1.0.0` releases
  that version. Before, a package that had released 0.1.0 had no way to
  reach 1.0.0. The version must come after the last release, and it
  cannot be a smaller change than the interface needs.

  ```sh
  wand p release 1.0.0
  ```

### Changed

- **`wand p release` names the kind of change: `breaking`, `feature` or
  `fix`.** Before 1.0, `wand p release major` gave 0.4.0 after 0.3.0, not
  1.0.0, and `minor` did the same as `patch`. The new words say what the
  change does, and wand chooses the number from it:

  ```sh
  wand p release breaking   # 0.3.1 -> 0.4.0, or 1.2.3 -> 2.0.0
  wand p release feature    # 0.3.1 -> 0.3.2, or 1.2.3 -> 1.3.0
  wand p release fix        # 0.3.1 -> 0.3.2, or 1.2.3 -> 1.2.4
  ```

  `major`, `minor` and `patch` still work, and print the new word.

- **`wand p upgrade`, `add` and `tidy` take only a version that your wand
  can use.** Each version of a package names the lowest wand it works
  with. Before, these commands could give you a version that needs a newer
  wand than the one you have, and then it did not run. Now they pass over
  that version and say why:

  ```
  github.com/mjstahl/json 1.5.0 needs wand 0.94.0 or later, before 1.0.0, and this is wand 0.93.1; keeping 1.4.0
  ```

  So a package can need a newer wand in a fix release. `wand p release`
  prints the new `wand` line, so that you see it when you release.
