# Count commits by author

Write a program that reads the output of `git log --format='%an|%s'` on
stdin and prints how many commits each author made. It is run as `prog`, or
as `prog N` to print only the first `N` authors.

Each line is one commit: the author's name, a `|`, then the subject. The
name is the text before the first `|`; it can hold spaces. The subject can
hold more `|` characters. Skip blank lines and lines with no `|`. Names are
compared exactly, so `ana` and `Ana` are two authors.

Print one line for each author, as `COUNT NAME`. Put the most commits first.
Authors with the same count go in ascending byte order of name (`Zoe` comes
before `ana`). With `N`, print only the first `N` of these lines.

`N` is a whole number, 1 or more. If it is not, print a message to stderr
and exit with code 2.

Example. For this input:

```
Ana Silva|Fix the parser
Bo|Add tests
Ana Silva|Release 1.2 | notes
```

the program prints:

```
2 Ana Silva
1 Bo
```
