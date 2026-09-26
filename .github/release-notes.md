## 0.80.7 - 2026-09-25

Two changes to the effects a file performs, and four fixes.

### Changed

- **An import performs what the imported file's bindings perform.** An
  import evaluates a module's bindings, so a binding that runs a command or
  reads a file does that work when another file imports it. The effects
  reached nobody: the importing file performed nothing, and a manifest that
  did not mention them still typechecked while the run did the work.

  ```
  -- b.wand
  uses {Shell(echo)}
  let greeting = $(echo hi)

  -- a.wand
  uses {IO}
  let {greeting} = import ./b

  -- before    typechecks, and running it runs `echo`
  -- now       type error: performs Shell, which the manifest does not allow
  ```

  Defining a function performs nothing on import — its effects sit on the
  arrow and arrive when something calls it — so a module of functions
  imports clean however much its functions do. Nothing in the standard
  library does work at load, so no existing file changes.

- **A contract makes a function raise.** A failed `requires` or `ensures`
  raises, and `try` catches it, but the effects said otherwise. The
  signature read `Int -> Int`, and `V-BANG2` called the `!` on such a
  function a promise of a risk that was not there. A contract now performs
  `Raise`, like everything else that can raise.

  ```
  let half n =
    requires n % 2 == 0
    n / 2

  -- before    half : Int -> Int
  -- now       half : Int -> Int ! {Raise}
  --           V-BANG1: 'half' can raise -- call it 'half!'
  ```

  A caller of a contract-carrying function performs `Raise` as well, so the
  names above it take the `!` too. `wand t --effects` is unchanged: it names
  the labels a manifest would, and `Raise` is not one of them.

### Fixed

- **`wand t --fix` deleted an import the file needed.** `V-IMP2` asked
  whether a name was mentioned in type position, not how often, so a module
  named once as a built-in type counted as unused however many times it was
  also called. `--fix` then deleted the import, hit the type error it had
  just made, reported that it had put the import back, and left the file
  without it.

  ```
  import IO
  import Path

  let show ((p, n): (Path, Int)) = IO.println "%{n} %{Path.to_string p}"

  -- before    V-IMP2: nothing in this file uses Path
  --           and --fix removed the line, leaving a file that does not
  --           typecheck
  -- now       silent; Path is called as well as named
  ```

  A module named *only* as a built-in type is still reported, which is what
  the rule is for.

- **A contract clause could not call a function.** `requires` and `ensures`
  read an expression with no application in it, so a call was cut in two:
  the name became the whole condition, and its argument started the body.

  ```
  let half n =
    requires even? n
    n / 2

  -- before    type error: expected Bool, got Int -> Bool
  --           the condition was `even?` and the body was `n`
  -- now       the condition is `even? n`, the body is `n / 2`
  ```

  `result` can be an argument now as well, so
  `ensures String.length result > 0` says what it looks like it says.

- **`wand f` wrote a lambda's contracts outside the lambda.** The first
  clause was hugged onto the `fn ... ->` line and the rest were written at
  the indent around the lambda, so the indentation said the body belonged
  to whatever held it.

  ```
  -- before
  let pair =
    fn a b -> requires a > 0
    requires b > 0
    a + b

  -- now
  let pair =
    fn a b ->
      requires a > 0
      requires b > 0
      a + b
  ```

- **`wand f` dropped the brackets around a handler arm's pattern.** A
  handler arm reads its pattern as a single item: an operation's argument is
  followed by the name of the continuation, and `return`'s pattern by the
  `->`. A constructor pattern printed without its brackets ran into the
  name beside it, and the file no longer parsed.

  ```
  handle work () with
  | Store!get (Key k) resume -> resume (lookup k)
  | return (Some x) -> x

  -- before    wand f wrote `| Store!get Key k resume ->` and
  --           `| return Some x ->`, neither of which parses
  -- now       the brackets stay
  ```
