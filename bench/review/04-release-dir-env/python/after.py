import os
import shutil

# Remove all but the five newest releases. Release directories are named
# by timestamp, so the newest sort last.
#
# RELEASE_DIR says where they are, so staging can use the same script.

RELEASES = os.environ.get("RELEASE_DIR", ".")
KEEP = 5

old = sorted(os.listdir(RELEASES), reverse=True)[KEEP:]
for name in old:
    path = os.path.join(RELEASES, name)
    print(f"removing {path}")
    shutil.rmtree(path)
