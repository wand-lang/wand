import heapq
import sys

# The twenty slowest requests in app.log: the lines with the highest ms=,
# and lines with the same time in byte order.


def key(line):
    return (-int(line.rsplit("ms=", 1)[1]), line)


with open(sys.argv[1] + "/app.log") as f:
    lines = f.read().splitlines()
for line in heapq.nsmallest(20, lines, key=key):
    print(line)
