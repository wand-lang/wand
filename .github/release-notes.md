## 0.93.1 - 2026-09-29

A broken wand.pkg can be repaired with `wand p tidy`.

### Fixed

- **`wand p tidy` repairs a `wand.pkg` whose sections cannot be read.**
  `tidy` writes the sections again. It keeps each sum line it can read, 
  so a hash already recorded is still checked, and it names the lines 
  it drops. Then `wand p interface` writes the interface section from the 
  code.

- **A clear error for text after the record that is in no section.** When
  the interface marker line was deleted, the interface lines became part
  of the record, and the error was `cons is '::'`. Now the error says that
  the text is in no section, and to run `wand p tidy`.

- **The error for a wrong sum hash names the other cause.** It said only
  that the module or the cache changed. Now it also says: if the sum
  section was changed by hand, restore it from version control, or remove
  the line and run `wand p tidy`.
