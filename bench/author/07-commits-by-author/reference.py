import re
import sys
from collections import Counter

if len(sys.argv) > 2 or (len(sys.argv) == 2 and not re.fullmatch(r"0*[1-9]\d*", sys.argv[1])):
    print("usage: prog [N], where N is 1 or more", file=sys.stderr)
    sys.exit(2)
top = int(sys.argv[1]) if len(sys.argv) == 2 else None

counts = Counter(line.split("|", 1)[0] for line in sys.stdin.read().splitlines() if "|" in line)
rows = sorted(counts.items(), key=lambda kv: (-kv[1], kv[0].encode()))
for name, n in rows[:top]:
    print(n, name)
