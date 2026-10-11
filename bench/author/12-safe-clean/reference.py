import os
import shutil
import sys

if len(sys.argv) < 2 or sys.argv[1] == "" or not os.path.isdir(sys.argv[1]):
    print("usage: prog TARGET, where TARGET is a directory", file=sys.stderr)
    sys.exit(2)
target = sys.argv[1]
cache = os.path.join(target, "cache")

if os.path.isdir(cache) and not os.path.islink(cache):
    for name in sorted(os.listdir(cache)):
        path = os.path.join(cache, name)
        if os.path.isdir(path) and not os.path.islink(path):
            shutil.rmtree(path)
        else:
            os.remove(path)
        print("removed", name)
else:
    print("nothing to clean")
print("kept", len(os.listdir(target)))
