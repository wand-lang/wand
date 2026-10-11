import glob
import subprocess

# Compress yesterday's rotated logs.

for path in sorted(glob.glob("logs/*.log.1")):
    subprocess.run(["gzip", "-9", path], check=True)
