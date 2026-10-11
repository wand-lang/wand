import re
import sys

# How many lines report a refused connection or a lock timeout, and the
# sum of their ms=.
pattern = re.compile(r"(connection refused|timeout waiting for lock).*ms=(\d+)$")
count = total = 0
with open(sys.argv[1] + "/app.log") as f:
    for line in f:
        m = pattern.search(line.rstrip("\n"))
        if m:
            count += 1
            total += int(m.group(2))
print(count, total)
