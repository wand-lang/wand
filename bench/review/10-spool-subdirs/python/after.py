import glob
import os
import time

# Remove the temporary files the uploader leaves anywhere under ./spool,
# once they are a day old. A younger one may be an upload in progress.

DAY = 24 * 3600


def a_day_old(path):
    return time.time() - os.path.getmtime(path) > DAY


def leftover(path):
    return path.endswith(".tmp") or a_day_old(path)


for path in glob.glob("spool/**/*", recursive=True):
    if os.path.isfile(path) and leftover(path):
        os.remove(path)
