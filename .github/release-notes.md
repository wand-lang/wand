## 0.95.0 - 2026-09-30

Interfaces work across modules, a record update uses its own module's type, and a function stored in a record field is charged where it is stored.

### Upgrading

One change can make a file fail that passed with 0.94.1. `wand t` names the line to add:

- A file that stores a function in a record field whose type writes no effects now performs what that function performs. If the function runs a command or writes a file, the file that builds the value declares it, as in `uses {Shell(git)}`. Before, no manifest saw the effect when the field was called from another module.

### Fixed

- **A function stored in a record field is charged where it is stored.** A field such as `f: Unit -> String` could hold a function that runs a command, and no manifest saw it: not the file that built the value, and not a file that called the field in another module. A program under `uses {IO}` ran the command. Now building the value performs what the stored function performs, except `Raise` (#61).

- **A record update uses the type of its own module.** With two modules that each declare a type of one name, such as `type State`, an update in one module could build the other module's type: a wrong value with no error, or `constructor 'State' has no field named ...`, depending on the order of the imports (#50).

- **An interface can name its own module's types, and be implemented from another module.** An interface `B(create: Int -> N)`, with `N` declared beside it, could not be implemented from another file. A type alias that names an imported interface was unknown in a third file. Two files that bound the interface's module under different names did not agree that a module fits it (#51).

- **A list can hold different modules that implement one interface.** `[ints, rev]` was refused with a message that named the interface twice. Now the list holds the interface both modules claim, inside tuples too, so `Map.from_list [("i", ints), ("r", rev)]` works (#52).

- **A continuation used after its case answered is an error, not a crash.** A handler that called `k` inside a function its case returned stopped wand with `Continuation_already_resumed`. Now the call says why, and calling `k` twice says so too (#53).

- **A function that stores a closure calling itself no longer takes on the closure's effects.** A builder whose field may raise came out raising, so `V-BANG1` asked for a `!` on a function that raises nothing (#54).
