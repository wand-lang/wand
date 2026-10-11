import sys

# The first five ERROR lines from the db component. The read stops there,
# so the rest of app.log is never read.
found = 0
with open(sys.argv[1] + "/app.log") as f:
    for line in f:
        words = line.split()
        if len(words) >= 3 and words[1] == "ERROR" and words[2] == "db:":
            print(line, end="")
            found += 1
            if found == 5:
                break
