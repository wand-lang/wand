# Report full disks

Write a program that reads the output of `df -P` on stdin and prints the
file systems that are too full. It is run as `prog PERCENT`.

The first line of the input is a header; skip it. Each other line has six
fields separated by one or more spaces: file system, total blocks, used
blocks, available blocks, capacity, and mount point. The capacity is a whole
number followed by `%`, such as `91%`. The mount point is everything after
the capacity field, and it can hold spaces (`/Volumes/My Disk`). Skip a line
that does not have this shape, and skip blank lines.

Print one line for each file system whose capacity is `PERCENT` or more, as
`CAPACITY% MOUNT`. Put the fullest first. File systems with the same capacity
go in ascending byte order of the mount point.

`PERCENT` is a whole number from 0 to 100. If it is missing or is not such a
number, print a message to stderr and exit with code 2.

Example. For `prog 80` and this input:

```
Filesystem     1024-blocks      Used Available Capacity Mounted on
/dev/disk1s1     488245288 450000000  38245288      93% /
/dev/disk1s4     488245288   2097172  38245288       6% /private/var/vm
/dev/disk2s1      15000000  12000000   3000000      80% /Volumes/My Disk
```

the program prints:

```
93% /
80% /Volumes/My Disk
```
