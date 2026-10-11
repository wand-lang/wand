import csv
import sys
from collections import defaultdict

# For each region and product in sales.csv: how many sales, their total
# amount, and their mean amount rounded down.
count = defaultdict(int)
total = defaultdict(int)
with open(sys.argv[1] + "/sales.csv", newline="") as f:
    for row in csv.DictReader(f):
        key = f"{row['region']} {row['product']}"
        count[key] += 1
        total[key] += int(row["amount"])
for key in sorted(count):
    print(key, count[key], total[key], total[key] // count[key])
