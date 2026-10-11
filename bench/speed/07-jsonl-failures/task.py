import json
import sys
from collections import defaultdict

# For each endpoint in events.jsonl: how many requests, how many failed,
# and the failures per thousand, rounded down.
total = defaultdict(int)
failed = defaultdict(int)
with open(sys.argv[1] + "/events.jsonl") as f:
    for line in f:
        event = json.loads(line)
        total[event["endpoint"]] += 1
        if not event["ok"]:
            failed[event["endpoint"]] += 1
for endpoint in sorted(total):
    t, n = total[endpoint], failed[endpoint]
    print(endpoint, t, n, n * 1000 // t)
