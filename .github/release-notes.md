## 0.95.0 - 2026-09-30

Interfaces work across modules, a function stored in a record field is charged where it is stored, `Shared.update` answers the old value, and strings can hold any byte.

### Upgrading

Two changes can make a file fail that passed with 0.94.1. `wand t` names each place:

- A file that stores a function in a record field whose type writes no effects now performs what that function performs. If the function runs a command or writes a file, the file that builds the value declares it, as in `uses {Shell(git)}`. Before, no manifest saw the effect when the field was called from another module.
- `Shared.update` answers a value. Where `Unit` is required, as in `if c then Shared.update s f else ()`, drop the value. A handler for `Shared!update` resumes with the old value.

### Added

- **Byte escapes: `\xNN`.** A string can hold any byte, as in `"\xff\xfb\x01"`. `wand f` keeps such bytes, and writes valid UTF-8 as the character. In a regex, `\xNN` works inside a character class too, so `r/[\xfb-\xfe]/` is a byte range (#56).

- **`wand f` takes a directory**, and formats every `.wand` file under it, as `wand t` and `wand s` read one (#57).

- **`Wand.check_at`: check text as the file it will be.** Its imports are read from beside the path, and what the check finds names the path, so an editor can check a buffer before it saves it (#59).

- **`Checked.effects`: what a source performs, as data**, each label as a `uses` line writes it, such as `["FS.Write", "Shell(git)"]` (#60).

### Changed

- **`Shared.update` answers the value from before the update**, as `getAndUpdate` does in Java. `Shared.update outbox (fn _ -> [])` answers the lines it removed, and `Shared.update counter (fn c -> c + 1)` answers the number this caller took. The new value is `f` of the old one (#55).

### Fixed

- **A function stored in a record field is charged where it is stored.** A field such as `f: Unit -> String` could hold a function that runs a command, and no manifest saw it: not the file that built the value, and not a file that called the field in another module. A program under `uses {IO}` ran the command (#61).

- **A record update uses the type of its own module.** With two modules that each declare `type State`, an update in one module could build the other module's type: a wrong value with no error, or `constructor 'State' has no field named ...`, depending on the order of the imports (#50).

- **An interface can name its own module's types, and be implemented from another module.** A type alias that names an imported interface works in any file, and two files that bind the interface's module under different names agree that a module fits it (#51).

- **A list can hold different modules that implement one interface**, inside tuples too, so `Map.from_list [("i", ints), ("r", rev)]` works (#52).

- **A continuation used after its case answered is an error, not a crash.** It stopped wand with `Continuation_already_resumed`. Calling `k` twice is an error too (#53).

- **A function that stores a closure calling itself no longer takes on the closure's effects**, so `V-BANG1` no longer asks for a `!` on a builder that raises nothing (#54).

### Documentation

- **Serving with no socket, in a test.** The reference shows how a handler serves fake connections through `Net!listen`, `Net!read_line` and `Net!write` (#58).
