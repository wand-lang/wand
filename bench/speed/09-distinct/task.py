import sys

# How many distinct addresses, distinct address and component pairs, and
# distinct lines app.log holds.
ips, pairs, lines = set(), set(), set()
with open(sys.argv[1] + "/app.log") as f:
    for line in f:
        line = line.rstrip("\n")
        words = line.split()
        ip = next((w for w in words if w.startswith("ip=")), None)
        if ip is not None:
            ips.add(ip)
            if len(words) >= 3:
                pairs.add(f"{ip} {words[2]}")
        lines.add(line)
print("ips", len(ips))
print("pairs", len(pairs))
print("lines", len(lines))
