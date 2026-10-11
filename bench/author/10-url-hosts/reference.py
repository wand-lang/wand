import re
import sys
from collections import Counter

hosts = Counter()
invalid = 0
for line in sys.stdin.read().splitlines():
    line = line.strip(" \t")
    if not line:
        continue
    m = re.match(r"https?://([^/?#]*)", line)
    host = m.group(1).split(":", 1)[0].lower() if m else ""
    if host:
        hosts[host] += 1
    else:
        invalid += 1

for host, n in sorted(hosts.items(), key=lambda kv: (-kv[1], kv[0])):
    print(n, host)
if invalid:
    print(invalid, "invalid")
