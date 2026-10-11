import os
import subprocess

# Rotate app.log when it is larger than 100MB, keeping two old
# generations. Not while app-server runs: it holds the file open and
# would go on writing into the renamed one.

LOG = "app.log"


def writer_running():
    return subprocess.run(["pgrep", "-x", "app-server"], capture_output=True).returncode == 0


def due():
    return os.path.isfile(LOG) and os.path.getsize(LOG) > 100 * 1000 * 1000


if writer_running():
    print("app-server is running; not rotating")
elif due():
    if os.path.isfile("app.log.1"):
        os.rename("app.log.1", "app.log.2")
    os.rename(LOG, "app.log.1")
    open(LOG, "w").close()
    print("rotated")
