import sys
from collections import Counter

# How many ERROR lines each component logged in each hour, by component
# and then by hour.
counts = Counter()
with open(sys.argv[1] + "/app.log") as f:
    for line in f:
        words = line.split()
        if len(words) >= 3 and words[1] == "ERROR":
            counts[f"{words[2][:-1]} {words[0][11:13]}"] += 1
for key in sorted(counts):
    print(key, counts[key])
