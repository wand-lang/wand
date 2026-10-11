# Find crashlooping pods

Write a program that reads a pod list, as `kubectl get pods -o json` prints
it, and prints the pods that restart too often. It is run as
`prog FILE THRESHOLD`.

The file holds one JSON object. Its `items` field is a list of pods. Each pod
has `metadata.name` and `metadata.namespace` (strings), and may have
`status.containerStatuses`, a list of containers, each with an integer
`restartCount`. A pod with no `status`, or with no `containerStatuses`, has
restarted 0 times. Other fields can be present; ignore them.

A pod's restarts are the sum of `restartCount` over its containers. Print one
line for each pod whose restarts are greater than `THRESHOLD`, as
`NAMESPACE/NAME RESTARTS`. Put the most restarts first. Pods with the same
restarts go in ascending order of `NAMESPACE/NAME`. If no pod is over the
threshold, print nothing.

`THRESHOLD` is a whole number, 0 or more. If an argument is missing or
`THRESHOLD` is not a whole number of 0 or more, print a message to stderr and
exit with code 2. If the file does not exist or is not valid JSON of this
shape, print a message to stderr and exit with code 1.

Example. For this file and `prog pods.json 3`:

```json
{"items": [
  {"metadata": {"name": "api-1", "namespace": "prod"},
   "status": {"containerStatuses": [{"name": "api", "restartCount": 7}]}},
  {"metadata": {"name": "web-1", "namespace": "prod"},
   "status": {"containerStatuses": [{"name": "web", "restartCount": 1}]}}
]}
```

the program prints:

```
prod/api-1 7
```
