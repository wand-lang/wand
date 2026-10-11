# Sum a CSV column

Write a program that prints the sum of one column of a CSV file. It is run as
`prog FILE COLUMN`.

The first line of the file is the header. It names the columns. The other
lines are rows. Fields are separated by commas. No field holds a comma, a
quote or a newline, so you do not need to handle CSV quoting. Ignore blank
lines. The file can end with or without a newline.

`COLUMN` is the name of a column in the header, matched exactly. Sum the
column's values over all rows and print the sum as a whole number. Each value
is a whole number, possibly negative (`-12`). An empty field counts as 0.

Errors:

- If an argument is missing, print a message to stderr and exit with code 2.
- If the file does not exist, print a message to stderr and exit with code 1.
- If no header field is `COLUMN`, print `no column: COLUMN` to stderr and exit
  with code 2.
- If a value in the column is not a whole number, print a message that names
  its line number (the header is line 1) to stderr, print nothing to stdout,
  and exit with code 1.

Example. For this file and `prog sales.csv amount`:

```
region,amount
eu,10
us,-3
eu,
```

the program prints `7`.
