import os
import socket
from collections import defaultdict

# Split app.log into one report per day under reports/, and write
# reports/summary.txt: the host, then each day with its line count.

by_day = defaultdict(list)
with open("app.log") as f:
    for line in f.read().splitlines():
        by_day[line[:10]].append(line)

os.makedirs("reports", exist_ok=True)
for day, lines in by_day.items():
    with open(f"reports/{day}.log", "w") as out:
        out.write("\n".join(lines))

summary = [f"{day} {len(lines)}" for day, lines in by_day.items()]
with open("reports/summary.txt", "w") as out:
    out.write("\n".join([socket.gethostname()] + summary))
