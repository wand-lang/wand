import re
import sys

if len(sys.argv) < 3:
    print("usage: prog FILE COLUMN", file=sys.stderr)
    sys.exit(2)
path, column = sys.argv[1], sys.argv[2]
try:
    with open(path) as f:
        lines = f.read().splitlines()
except OSError as e:
    print(e, file=sys.stderr)
    sys.exit(1)

numbered = [(n, line) for n, line in enumerate(lines, 1) if line != ""]
if not numbered:
    print(f"no column: {column}", file=sys.stderr)
    sys.exit(2)
header = numbered[0][1].split(",")
if column not in header:
    print(f"no column: {column}", file=sys.stderr)
    sys.exit(2)
index = header.index(column)

total = 0
for n, line in numbered[1:]:
    fields = line.split(",")
    value = fields[index] if index < len(fields) else ""
    if value == "":
        continue
    if not re.fullmatch(r"-?\d+", value):
        print(f"line {n}: not a whole number: {value}", file=sys.stderr)
        sys.exit(1)
    total += int(value)
print(total)
