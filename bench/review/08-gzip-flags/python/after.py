import glob
import os
import subprocess

# Compress yesterday's rotated logs. GZIP_FLAGS adds flags of its own,
# such as -1 on a busy host; the default is -9.

FLAGS = os.environ.get("GZIP_FLAGS", "-9")

for path in sorted(glob.glob("logs/*.log.1") + glob.glob("logs/*.txt.1")):
    subprocess.run(f"gzip {FLAGS} {path}", shell=True, check=True)
