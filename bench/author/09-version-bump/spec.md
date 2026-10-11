# Bump the version

Write a program that raises the version number in the file `VERSION` in the
current directory. It is run as `prog PART`, where `PART` is `major`,
`minor` or `patch`.

`VERSION` holds one version, `MAJOR.MINOR.PATCH`, and can end with a
newline. Each of the three parts is `0` or a whole number with no leading
zero (`1.10.0` is valid; `1.02.0`, `1.2`, `v1.2.3` and `1.2.3-rc.1` are not).
Spaces, tabs and newlines at the start or end of the file do not count.

- `major` adds 1 to `MAJOR` and sets `MINOR` and `PATCH` to 0.
- `minor` adds 1 to `MINOR` and sets `PATCH` to 0.
- `patch` adds 1 to `PATCH`.

Write the new version back to `VERSION`, followed by one newline, and print
it.

Errors:

- If `PART` is missing or is not one of the three words, print a message to
  stderr and exit with code 2. Do not change the file.
- If `VERSION` does not exist or does not hold a valid version, print a
  message to stderr and exit with code 1. Do not change the file.

If both are wrong, `PART` is checked first.

Example. If `VERSION` holds `1.4.2`, then `prog minor` changes it to `1.5.0`
and prints:

```
1.5.0
```
