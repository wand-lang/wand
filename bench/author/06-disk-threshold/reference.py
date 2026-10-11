import re
import sys

LINE = re.compile(r"(\S+) +(\d+) +(\d+) +(\d+) +(\d+)% +(.+)")

if len(sys.argv) < 2 or not re.fullmatch(r"\d+", sys.argv[1]) or int(sys.argv[1]) > 100:
    print("usage: prog PERCENT, from 0 to 100", file=sys.stderr)
    sys.exit(2)
limit = int(sys.argv[1])

full = []
for line in sys.stdin.read().splitlines()[1:]:
    m = LINE.fullmatch(line.strip())
    if m and int(m.group(5)) >= limit:
        full.append((int(m.group(5)), m.group(6)))
for capacity, mount in sorted(full, key=lambda f: (-f[0], f[1])):
    print(f"{capacity}% {mount}")
