import os
import re
import sys

BACKUP = re.compile(r"backup-\d{4}-\d{2}-\d{2}\.tar")

if len(sys.argv) < 3 or not re.fullmatch(r"\d+", sys.argv[2]):
    print("usage: prog DIR N, where N is a whole number", file=sys.stderr)
    sys.exit(2)
directory, keep = sys.argv[1], int(sys.argv[2])
if not os.path.isdir(directory):
    print(f"not a directory: {directory}", file=sys.stderr)
    sys.exit(1)

backups = sorted(
    name
    for name in os.listdir(directory)
    if BACKUP.fullmatch(name) and os.path.isfile(os.path.join(directory, name))
)
old = backups[: max(len(backups) - keep, 0)]
for name in old:
    os.remove(os.path.join(directory, name))
for name in old:
    print("deleted", name)
print("kept", len(backups) - len(old))
