import json
import re
import sys

if len(sys.argv) < 3 or not re.fullmatch(r"\d+", sys.argv[2]):
    print("usage: prog FILE THRESHOLD", file=sys.stderr)
    sys.exit(2)
threshold = int(sys.argv[2])
try:
    with open(sys.argv[1]) as f:
        pods = json.load(f)["items"]
    rows = []
    for pod in pods:
        meta = pod["metadata"]
        containers = (pod.get("status") or {}).get("containerStatuses") or []
        restarts = sum(int(c["restartCount"]) for c in containers)
        rows.append((f"{meta['namespace']}/{meta['name']}", restarts))
except (OSError, ValueError, KeyError, TypeError) as e:
    print(f"cannot read pods: {e}", file=sys.stderr)
    sys.exit(1)

for name, restarts in sorted(rows, key=lambda r: (-r[1], r[0])):
    if restarts > threshold:
        print(name, restarts)
