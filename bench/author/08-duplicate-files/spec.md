# Find duplicate files

Write a program that finds the files in a directory tree that hold the same
bytes. It is run as `prog DIR`.

Look at every regular file in `DIR` and in all directories below it, also
files whose names start with `.`. Two files are duplicates when their
contents are byte for byte the same; two empty files are duplicates too. The
file names do not matter.

Print each group of two or more duplicates. Write each file as its path
relative to `DIR`, with `/` between the parts (`logs/a.txt`). Print the paths
of one group one per line, in ascending byte order. Put one blank line
between two groups. Order the groups by their first path, in ascending byte
order. If there are no duplicates, print nothing.

If the argument is missing, print a message to stderr and exit with code 2.
If `DIR` is not a directory, print a message to stderr and exit with code 1.

Example. If `DIR` holds `a.txt` and `logs/b.txt`, both holding `hello`, and
`c.txt` and `d.txt`, both holding `bye`, and `e.txt` holding `other`, the
program prints:

```
a.txt
logs/b.txt

c.txt
d.txt
```
