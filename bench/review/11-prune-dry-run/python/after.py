import subprocess
import sys

# Remove dangling images older than a week.
#
# --dry-run lists what would go, and removes nothing.

AGE = "until=168h"


def docker(*args):
    return subprocess.run(["docker", *args], capture_output=True, text=True, check=True).stdout.strip()


if "--dry-run" in sys.argv[1:]:
    print(docker("image", "ls", "-q", "--filter", "dangling=true", "--filter", AGE))
else:
    print(docker("image", "prune", "-f", "--filter", AGE))
