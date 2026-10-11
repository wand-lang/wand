import os

# Rotate app.log when it is larger than 100MB.

LOG = "app.log"

if os.path.isfile(LOG) and os.path.getsize(LOG) > 100 * 1000 * 1000:
    os.rename(LOG, "app.log.1")
    open(LOG, "w").close()
    print("rotated")
