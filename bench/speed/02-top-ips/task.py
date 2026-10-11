import sys
from collections import Counter

# The ten addresses that appear most often in app.log, most first.
counts = Counter()
with open(sys.argv[1] + "/app.log") as f:
    for line in f:
        for word in line.split():
            if word.startswith("ip="):
                counts[word[3:]] += 1
                break
for ip, n in sorted(counts.items(), key=lambda kv: (-kv[1], kv[0]))[:10]:
    print(ip, n)
