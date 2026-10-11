import datetime
import os
import subprocess

# Archive the data directory into backups/, named by the date.

stamp = datetime.date.today().isoformat()
archive = f"backups/data-{stamp}.tar.gz"

os.makedirs("backups", exist_ok=True)
subprocess.run(["tar", "-czf", archive, "./data"], check=True)
print(archive)
