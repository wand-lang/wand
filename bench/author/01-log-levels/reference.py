import sys

LEVELS = ["ERROR", "WARN", "INFO", "DEBUG"]

if len(sys.argv) < 2:
    print("usage: prog LOGFILE", file=sys.stderr)
    sys.exit(2)
try:
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()
except OSError as e:
    print(e, file=sys.stderr)
    sys.exit(1)

counts = {}
for line in lines:
    words = line.split()
    if len(words) >= 2 and words[1] in LEVELS:
        counts[words[1]] = counts.get(words[1], 0) + 1
for level in sorted(counts, key=lambda l: (-counts[l], LEVELS.index(l))):
    print(level, counts[level])
