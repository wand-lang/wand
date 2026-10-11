import datetime
import os
import subprocess
import sys

# Archive the data directory into backups/, named by the date.
#
# --verbose also prints the size of the archive.

verbose = "--verbose" in sys.argv[1:]

stamp = datetime.date.today().isoformat()
archive = f"backups/data-{stamp}.tar.gz"

if not os.path.isdir("data"):
    print("nothing to back up: ./data does not exist", file=sys.stderr)
    sys.exit(1)

os.makedirs("backups", exist_ok=True)
subprocess.run(["tar", "-czf", archive, "./data"], check=True)
print(archive)

if verbose:
    print(f"size: {os.path.getsize(archive)}")
