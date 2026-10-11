import re
import sys

PARTS = ["major", "minor", "patch"]
VERSION = re.compile(r"(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)")

if len(sys.argv) != 2 or sys.argv[1] not in PARTS:
    print("usage: prog major|minor|patch", file=sys.stderr)
    sys.exit(2)
try:
    with open("VERSION") as f:
        text = f.read().strip()
except OSError as e:
    print(e, file=sys.stderr)
    sys.exit(1)
m = VERSION.fullmatch(text)
if not m:
    print(f"not a version: {text!r}", file=sys.stderr)
    sys.exit(1)

major, minor, patch = (int(g) for g in m.groups())
if sys.argv[1] == "major":
    major, minor, patch = major + 1, 0, 0
elif sys.argv[1] == "minor":
    minor, patch = minor + 1, 0
else:
    patch += 1
new = f"{major}.{minor}.{patch}"
with open("VERSION", "w") as f:
    f.write(new + "\n")
print(new)
