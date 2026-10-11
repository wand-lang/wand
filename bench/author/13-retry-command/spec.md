# Retry a flaky command

Write a program that runs a command that sometimes fails, and tries it again
until it succeeds or the attempts run out. It is run as `prog N`, in a
directory that holds the script `flaky.sh`.

Run the command `sh flaky.sh` in the current directory, at most `N` times.
Stop at the first attempt that exits with code 0. Do not wait between
attempts.

For each attempt that fails, print `attempt K: exit C`, where `K` counts from
1 and `C` is the command's exit code. For the attempt that succeeds, print
`attempt K: ok`, then `output: TEXT`, where `TEXT` is what the command wrote
to stdout, without its final newline. Do not print what the command writes
to stderr on stdout.

If all `N` attempts fail, print `gave up after N attempts` and exit with
code 1. Otherwise exit with code 0.

`N` is a whole number, 1 or more. If it is missing or is not such a number,
print a message to stderr, do not run the command, and exit with code 2.

Example. If the command fails twice with exit code 3 and then succeeds and
prints `ok on try 3`, then `prog 5` prints:

```
attempt 1: exit 3
attempt 2: exit 3
attempt 3: ok
output: ok on try 3
```

The `flaky.sh` here succeeds on the attempt number that the file `need`
holds. Use it to test, but do not depend on how it works.
