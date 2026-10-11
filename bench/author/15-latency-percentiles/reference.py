import json
import sys


def record(line):
    try:
        r = json.loads(line)
    except ValueError:
        return None
    if not isinstance(r, dict):
        return None
    endpoint, ms = r.get("endpoint"), r.get("ms")
    if not isinstance(endpoint, str) or type(ms) is not int or ms < 0:
        return None
    return endpoint, ms


def rank(p, n):
    return max((p * n + 99) // 100, 1)


latencies = {}
skipped = 0
for line in sys.stdin.read().splitlines():
    if not line.strip():
        continue
    r = record(line)
    if r is None:
        skipped += 1
    else:
        latencies.setdefault(r[0], []).append(r[1])

for endpoint in sorted(latencies, key=str.encode):
    v = sorted(latencies[endpoint])
    n = len(v)
    print(f"{endpoint} count={n} p50={v[rank(50, n) - 1]} p95={v[rank(95, n) - 1]}")
if skipped:
    print("skipped", skipped)
