# Latency percentiles

Write a program that reads request records on stdin, one JSON object per
line, and prints the median and the 95th percentile latency of each
endpoint. It is run as `prog`, with no arguments.

A valid line is a JSON object with a field `endpoint` that is a string and a
field `ms` that is a whole number, 0 or more (`12`, not `12.5`, `"12"`,
`-3` or `true`). Other fields can be present; ignore them. Skip blank lines.
Count every other line as skipped: text that is not JSON, JSON that is not
an object, and objects with a missing or wrong `endpoint` or `ms`.

For each endpoint, sort its `ms` values in ascending order, v1 to vn. The
P-th percentile is v at rank r, where r is P × n / 100 rounded up to a whole
number (the nearest-rank method). So for n = 4, p50 is v2 and p95 is v4.

Print one line for each endpoint, in ascending byte order of the endpoint,
as `ENDPOINT count=N p50=X p95=Y`. Then, if any line was skipped, print
`skipped K`, where `K` is the number of skipped lines.

Example. For this input:

```
{"endpoint": "/login", "ms": 30}
{"endpoint": "/login", "ms": 10}
{"endpoint": "/", "ms": 5, "status": 200}
not json
{"endpoint": "/login", "ms": 20}
```

the program prints:

```
/ count=1 p50=5 p95=5
/login count=3 p50=20 p95=30
skipped 1
```
