# Count log lines by level

Write a program that reads the log file named by its one argument and prints
how many lines carry each level.

A log line looks like this:

```
2026-10-01T12:00:03Z ERROR db: connection refused
```

The level is the second word of the line. The levels are `ERROR`, `WARN`,
`INFO` and `DEBUG`. Ignore a line whose second word is none of these, and
ignore blank lines.

Print one line for each level that occurs at least once, as `LEVEL COUNT`,
with the highest count first. Two levels with the same count go in the order
`ERROR`, `WARN`, `INFO`, `DEBUG`.

If the argument is missing, print `usage: prog LOGFILE` to stderr and exit
with code 2. If the file does not exist, print a message to stderr and exit
with code 1.

Example. For a file holding:

```
2026-10-01T12:00:01Z INFO web: started
2026-10-01T12:00:02Z ERROR db: connection refused
2026-10-01T12:00:03Z INFO web: request
```

the program prints:

```
INFO 2
ERROR 1
```
