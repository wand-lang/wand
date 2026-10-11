import re
import sys

DURATION = re.compile(r"(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?")


def show(seconds):
    h, m, s = seconds // 3600, seconds // 60 % 60, seconds % 60
    text = "".join(f"{n}{u}" for n, u in ((h, "h"), (m, "m"), (s, "s")) if n)
    return text or "0s"


totals = {}
for n, line in enumerate(sys.stdin.read().splitlines(), 1):
    fields = line.split()
    if not fields:
        continue
    m = DURATION.fullmatch(fields[1]) if len(fields) == 2 else None
    if not m or not fields[1]:
        print(f"line {n}: expected NAME DURATION, got {line!r}", file=sys.stderr)
        sys.exit(1)
    h, mi, s = (int(g or 0) for g in m.groups())
    totals[fields[0]] = totals.get(fields[0], 0) + h * 3600 + mi * 60 + s

for name in sorted(totals):
    print(name, show(totals[name]))
print("total", show(sum(totals.values())))
