import sys
from collections import Counter

# How many lines of app.log carry each level, most first.
counts = Counter()
with open(sys.argv[1] + "/app.log") as f:
    for line in f:
        words = line.split()
        if len(words) >= 2:
            counts[words[1]] += 1
for level, n in sorted(counts.items(), key=lambda kv: (-kv[1], kv[0])):
    print(level, n)
