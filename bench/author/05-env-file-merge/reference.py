import sys


def read(path):
    settings = {}
    with open(path) as f:
        for line in f.read().splitlines():
            stripped = line.strip(" \t")
            if stripped == "" or stripped.startswith("#") or "=" not in line:
                continue
            key, value = line.split("=", 1)
            key = key.strip(" \t")
            if key:
                settings[key] = value
    return settings


if len(sys.argv) < 3:
    print("usage: prog BASE OVERRIDE", file=sys.stderr)
    sys.exit(2)
try:
    merged = read(sys.argv[1])
    merged.update(read(sys.argv[2]))
except OSError as e:
    print(e, file=sys.stderr)
    sys.exit(1)
for key in sorted(merged):
    print(f"{key}={merged[key]}")
