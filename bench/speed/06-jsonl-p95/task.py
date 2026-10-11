import json
import sys
from collections import defaultdict

# For each endpoint in events.jsonl: how many requests, and the 95th
# percentile of their ms, by nearest rank.
times = defaultdict(list)
with open(sys.argv[1] + "/events.jsonl") as f:
    for line in f:
        event = json.loads(line)
        times[event["endpoint"]].append(event["ms"])
for endpoint in sorted(times):
    xs = sorted(times[endpoint])
    rank = (95 * len(xs) + 99) // 100
    print(endpoint, len(xs), xs[rank - 1])
