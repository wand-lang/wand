import os
import shutil
import time

# Remove build artifacts older than a week from the cache directory.

CACHE = "/var/cache/builds"
WEEK = 7 * 24 * 3600


def stale(path):
    return time.time() - os.path.getmtime(path) > WEEK


for name in os.listdir(CACHE):
    path = os.path.join(CACHE, name)
    if stale(path):
        shutil.rmtree(path, ignore_errors=True)
