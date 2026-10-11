import os
import sys
from collections import defaultdict

if len(sys.argv) < 2:
    print("usage: prog DIR", file=sys.stderr)
    sys.exit(2)
root = sys.argv[1]
if not os.path.isdir(root):
    print(f"not a directory: {root}", file=sys.stderr)
    sys.exit(1)

by_content = defaultdict(list)
for dirpath, _, names in os.walk(root):
    for name in names:
        path = os.path.join(dirpath, name)
        if os.path.isfile(path):
            with open(path, "rb") as f:
                by_content[f.read()].append(os.path.relpath(path, root).replace(os.sep, "/"))

groups = sorted(sorted(paths, key=str.encode) for paths in by_content.values() if len(paths) > 1)
print("\n\n".join("\n".join(g) for g in groups))
