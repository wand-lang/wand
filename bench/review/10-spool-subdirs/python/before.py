import glob
import os

# Remove the temporary files the uploader leaves in ./spool.

for path in glob.glob("spool/*.tmp"):
    os.remove(path)
