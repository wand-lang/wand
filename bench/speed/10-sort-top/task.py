import csv
import sys

# The hundred largest sales in sales.csv, largest first, and sales of the
# same amount by id.
with open(sys.argv[1] + "/sales.csv", newline="") as f:
    sales = [(int(r["id"]), int(r["amount"])) for r in csv.DictReader(f)]
sales.sort(key=lambda s: (-s[1], s[0]))
for id, amount in sales[:100]:
    print(id, amount)
