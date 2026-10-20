## 0.108.0 - 2026-10-20

This release adds two Regex functions and changes how an arm reads a `;`. One change can make an existing file behave differently: read Upgrading.

### Upgrading

- A `;` on the line of a match or handle arm that starts its own line now keeps the statement after it in the arm. Before, it ended the definition, and that statement ran at the top level. To keep the old reading, end the line after the arm's body and put the statement on the next line (#190).

### Added

- `Regex.replace_with` and `Regex.replace_all_with` replace a match with what a function makes of its captures (#198).
- `"\u{2192}"` writes a character by its code point (#197).

### Changed

- On a match or handle arm that starts its own line, a `;` keeps the next statement in the arm, so `| Error why -> log why; 0` has the value `0` (#190).
- A field read, such as `r.name`, waits for a later use to give the record's type, as a tuple read does (#193).
- `Fn(name, short, line = 1)` reads two or more bare names before a named field as puns (#194).
- A pattern's argument can be a constructor reached through a module, such as `Some M.Section(body)` (#195).
- `wand f` writes a bidirectional control or a zero-width character in a string as `\u{...}` (#197).

### Fixed

- A type declaration with a lower-case name says that a type name starts with a capital letter, and gives the name to write (#196).
- An unknown escape in a string lists the escapes there are (#197).
- The error for `%{` in a backtick string names `%{"\%{"}` as the way to write it (#199).
- A declaration over a built-in type's name lists the built-in type names (#200).

### Documentation

- The reference lists the built-in type names (#200).
