import hashlib
import os
import sys
from collections import Counter

# How many groups of files under tree/ hold the same bytes, and how many
# files are in those groups.
counts = Counter()
for root, _, files in os.walk(sys.argv[1] + "/tree"):
    for name in files:
        with open(os.path.join(root, name), "rb") as f:
            counts[hashlib.sha256(f.read()).hexdigest()] += 1
groups = [n for n in counts.values() if n > 1]
print("groups", len(groups), "files", sum(groups))
