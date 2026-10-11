# Add up durations

Write a program that reads lines of `NAME DURATION` on stdin and prints the
total time for each name. It is run as `prog`, with no arguments.

Each line holds a name and a duration, separated by one or more spaces.
Spaces at the start or end of a line do not count. A name holds no spaces
and is never `total`. A duration is one or more parts, in this order and
each at most once: whole hours followed by `h`, whole minutes followed by
`m`, whole seconds followed by `s`. So `1h30m`, `45s`, `2h5s` and `90m` are
durations, and `30m1h`, `1h1h`, `1.5h`, `10` and `5d` are not. Skip blank
lines.

If a line is not of this form, print `line N: ` and a message to stderr,
where `N` is its line number (the first line is 1), print nothing to stdout,
and exit with code 1.

Otherwise, print one line for each name, as `NAME TIME`, in ascending byte
order of name. Then print `total TIME`, the sum over all lines. Write `TIME`
as hours, minutes and seconds with minutes and seconds below 60, as
`XhYmZs`, leaving out each part that is 0: `1h30m`, `2h5s`, `45s`. Write a
time of 0 as `0s`.

Example. For this input:

```
build 1h30m
test 45s
build 40m
```

the program prints:

```
build 2h10m
test 45s
total 2h10m45s
```
