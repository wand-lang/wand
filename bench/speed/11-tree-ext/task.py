import os
import sys
from collections import defaultdict

# For each file extension under tree/: how many files, and their total
# size in bytes.
count = defaultdict(int)
size = defaultdict(int)
for root, _, files in os.walk(sys.argv[1] + "/tree"):
    for name in files:
        ext = os.path.splitext(name)[1]
        count[ext] += 1
        size[ext] += os.path.getsize(os.path.join(root, name))
for ext in sorted(count):
    print(ext, count[ext], size[ext])
