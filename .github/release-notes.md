## 0.84.0 - 2026-09-26

Talking to a command that keeps running, and reading a command's stderr as a
stream.


### Added

- **`Shell.spawn`: talk to a command that keeps running.** A REPL, a
  language server, a worker that takes jobs on stdin: the command runs for
  the length of a `with`, and the script writes to it and reads from it as
  it goes.

  ```
  uses {Shell(python3)}

  with Shell.spawn $*(python3 -i -q) as py -> (
    Shell.write! "print(6 * 7)\n" py;
    Shell.read_line py
  )
  -- Some("42")
  ```

  `Shell.read_line` and `Shell.read n` read stdout, `Shell.read_err_line`
  and `Shell.read_err n` read stderr apart from it, and `Shell.write` and
  `Shell.write!` write to stdin. `Shell.close` closes stdin, waits, and
  answers the exit code with whatever was not read:

  ```
  with Shell.spawn $*(sh -c "cat; exit 3") as p ->
    (Shell.write! "left\n" p; Shell.close p)
  -- ShellResult("left\n", "", 3)
  ```

  The `with` stops a command that was not closed. A read waits without
  holding up other work, so `Par.timeout` bounds one, and a command fed and
  read at once is two branches of `Par.all!`. Under `--dry-run` nothing is
  started and every read answers nothing.

- **`Shell.stream_err`: a command's stderr, line by line**, as
  `Shell.stream` reads its stdout.

  ```
  Shell.stream_err $*(sh -c "echo out; echo err >&2") |> Stream.to_list
  -- out
  -- ["err"]
  ```
