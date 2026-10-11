import re
import subprocess
import sys

if len(sys.argv) != 2 or not re.fullmatch(r"0*[1-9]\d*", sys.argv[1]):
    print("usage: prog N, where N is 1 or more", file=sys.stderr)
    sys.exit(2)
attempts = int(sys.argv[1])

for k in range(1, attempts + 1):
    r = subprocess.run(["sh", "flaky.sh"], stdout=subprocess.PIPE, text=True)
    if r.returncode == 0:
        print(f"attempt {k}: ok")
        print("output:", r.stdout.removesuffix("\n"))
        sys.exit(0)
    print(f"attempt {k}: exit {r.returncode}")
print(f"gave up after {attempts} attempts")
sys.exit(1)
