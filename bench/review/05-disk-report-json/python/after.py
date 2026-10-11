import json
import subprocess
import sys

# Print every mount that is more than 80% full.
#
# --json prints them as a JSON list instead, for the monitoring job.


def parse(line):
    words = line.split()
    if len(words) < 6:
        return None
    try:
        return {"mount": words[5], "used": int(words[4].replace("%", ""))}
    except ValueError:
        return None


out = subprocess.run(["df", "-Pk"], capture_output=True, text=True, check=True).stdout
mounts = [m for m in (parse(l) for l in out.splitlines()[1:]) if m]
full = [m for m in mounts if m["used"] > 80]

if "--json" in sys.argv[1:]:
    print(json.dumps(full))
else:
    for m in full:
        print(f"{m['mount']} {m['used']}%")
