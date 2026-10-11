# Rotate backups

Write a program that keeps the newest backups in a directory and deletes the
others. It is run as `prog DIR N`.

A backup is a regular file in `DIR` (not below it) whose name is exactly
`backup-YYYY-MM-DD.tar`, where `YYYY`, `MM` and `DD` are digits. The date
in the name says how new the backup is. Ignore every other file, also
`backup-2026-01-01.tar.gz` and `backup-latest.tar`.

Keep the `N` newest backups and delete the rest. Then print one line for each
deleted file, `deleted NAME`, in ascending order of name. Then print one last
line, `kept K`, where `K` is the number of backups that are left.

`N` is a whole number, 0 or more. If an argument is missing, or `N` is not a
whole number of 0 or more (for example `-1`, `two` or `1.5`), print a message
to stderr, delete nothing, and exit with code 2. If `DIR` is not a
directory, print a message to stderr and exit with code 1.

Example. If `DIR` holds `backup-2026-01-01.tar`, `backup-2026-02-01.tar`,
`backup-2026-03-01.tar` and `notes.txt`, then `prog DIR 2` deletes one file
and prints:

```
deleted backup-2026-01-01.tar
kept 2
```
