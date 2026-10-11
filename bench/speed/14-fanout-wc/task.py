import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

# Run wc -l once for each file under tree/d0/, eight at a time, and add up
# the lines: the cost of starting many small commands.


def lines(path):
    with open(path) as f:
        return int(subprocess.run(["wc", "-l"], stdin=f, capture_output=True, text=True).stdout)


files = [os.path.join(root, name) for root, _, names in os.walk(sys.argv[1] + "/tree/d0") for name in names]
with ThreadPoolExecutor(8) as pool:
    total = sum(pool.map(lines, files))
print("files", len(files), "lines", total)
