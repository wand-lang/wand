# Clean a deploy cache

Write a program that empties the cache directory of a deploy target. It is
run as `prog TARGET`. It runs unattended after each deploy, so it must never
remove anything outside `TARGET/cache`.

`TARGET` must be given, must not be the empty string, and must name an
existing directory. If it is not, print a message to stderr, remove nothing,
and exit with code 2.

If `TARGET/cache` does not exist or is not a directory, print
`nothing to clean`.

Otherwise, remove everything inside `TARGET/cache`: files, directories with
all they hold, and names that start with `.`. Keep the `cache` directory
itself. Print `removed NAME` for each entry that was directly inside
`cache`, in ascending byte order of `NAME`.

In both cases, print one last line, `kept K`, where `K` is the number of
entries directly inside `TARGET` after the clean, `cache` included and names
that start with `.` included. Exit with code 0.

Example. If `TARGET` holds `app.jar`, `config/` and `cache/`, and `cache`
holds `a.tmp` and `pages/`, then `prog TARGET` leaves `TARGET/cache` empty
and prints:

```
removed a.tmp
removed pages
kept 3
```
