# Count URLs by host

Write a program that reads URLs on stdin, one per line, and prints how many
point at each host. It is run as `prog`, with no arguments.

Remove spaces and tabs from both ends of each line. Skip a line that is then
empty. No line holds `@`.

A line is a valid URL when it starts with `http://` or `https://` (in lower
case, exactly). Its host is the text after `://` up to the first `/`, `?` or
`#`, or to the end of the line if there is none of these. If the host holds
a `:`, remove the `:` and everything after it (that is the port). Write the
host in lower case. If the host is then empty, the line is not a valid URL.

Print one line for each host, as `COUNT HOST`. Put the highest count first.
Hosts with the same count go in ascending byte order. Then, if any line was
not a valid URL, print one more line, `COUNT invalid`, where `COUNT` is the
number of such lines. This line is always last.

Example. For this input:

```
https://example.com/a
http://Example.com:8080/b?x=1
ftp://files.example.com/
https://api.example.com
```

the program prints:

```
2 example.com
1 api.example.com
1 invalid
```
