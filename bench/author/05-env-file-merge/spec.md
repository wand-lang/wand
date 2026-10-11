# Merge two env files

Write a program that merges two files of settings. It is run as
`prog BASE OVERRIDE`.

Each file holds lines of the form `KEY=VALUE`. The key is the text before the
first `=`, with spaces and tabs removed from both of its ends. The value is
all the text after the first `=`, unchanged; it can be empty and can hold
more `=` signs. Ignore blank lines, lines that hold only spaces and tabs,
lines whose first character other than a space or a tab is `#`, and lines
that hold no `=`. Ignore a line whose key is empty.

A key in `OVERRIDE` replaces the same key in `BASE`. In one file, a later
line replaces an earlier line with the same key.

Print each key once, as `KEY=VALUE`, in ascending byte order of the key
(`B` comes before `a`).

If an argument is missing, print a message to stderr and exit with code 2.
If a file does not exist, print a message to stderr and exit with code 1.

Example. With `BASE`:

```
# defaults
PORT=8080
HOST=localhost
```

and `OVERRIDE`:

```
PORT=9090
DSN=postgres://db/app?sslmode=require
```

the program prints:

```
DSN=postgres://db/app?sslmode=require
HOST=localhost
PORT=9090
```
