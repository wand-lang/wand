import os
import sys

if len(sys.argv) != 4 or sys.argv[2] == "":
    print("usage: prog DIR OLD NEW, where OLD is not empty", file=sys.stderr)
    sys.exit(2)
root, old, new = sys.argv[1:]
if not os.path.isdir(root):
    print(f"not a directory: {root}", file=sys.stderr)
    sys.exit(1)

changed = []
for dirpath, _, names in os.walk(root):
    for name in names:
        path = os.path.join(dirpath, name)
        if not name.endswith(".txt") or not os.path.isfile(path):
            continue
        with open(path, "rb") as f:
            text = f.read()
        n = text.count(old.encode())
        if n:
            with open(path, "wb") as f:
                f.write(text.replace(old.encode(), new.encode()))
            changed.append((os.path.relpath(path, root).replace(os.sep, "/"), n))

for path, n in sorted(changed, key=lambda c: c[0].encode()):
    print(f"changed {path} ({n})")
print(f"changed {len(changed)} files, {sum(n for _, n in changed)} replacements")
