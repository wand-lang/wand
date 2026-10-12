# Benchmarks

Four benchmarks. Each one compares wand with Python, and with bash where
bash can do the work, on the same work.

| Part | Question | Runner | Who runs it |
|---|---|---|---|
| Speed | How long does the same task take? | `bench/speed.wand` | A person, before a release |
| Serve | How many requests per second does the same HTTP server answer? | `bench/serve.wand` | A person, before a release |
| Authoring | How many turns does an agent need to write a correct script, and how many defects get past its own checks? | `bench/author.wand` | A person or a schedule; it costs money |
| Review | Does a reviewer find an effect that a diff hides? | `bench/review.wand` | A person or a schedule; it costs money |

`bench/check.wand` does not call a model. It checks that each authoring
reference passes its hidden cases, that the three programs of each speed task
agree, that each server gives the right answer, and that each review item
typechecks and parses. CI does not run it.
Run it after a change to the language, and before a benchmark run, so that a
task that the change broke is found before it costs a run.

Run each from the repository root:

```bash
W=$PWD/_build/default/bin/wand.exe
WAND=$W $W bench/check.wand                      # the references still pass
WAND=$W $W bench/speed.wand                      # LINES, ROUNDS, TASKS
WAND=$W $W bench/serve.wand                      # REQUESTS, CONNECTIONS, ROUNDS, TASKS
WAND=$W MODEL=sonnet $W bench/author.wand        # RUNS, TASKS, LANGS, BUDGET, WORKERS
MODEL=sonnet $W bench/review.wand                # RUNS, ITEMS, WORKERS
```

Each runner writes its results to `bench/results/`, named by date (and by
model for the two that use one). Commit them: a result is useful only beside
the one before it.

## Speed: `bench/speed/<NN-slug>/`

- `task.wand`, `task.py`, `task.sh` — one program per language. Each takes
  one argument, the data directory, and prints its answer to stdout.
- The data comes from `bench/speed/gen.wand`: `app.log`, `events.jsonl` and
  `sales.csv` with `LINES` lines each, and a tree of small files under
  `tree/`. The generator is written out, not `Random`, so a run next month
  reads the same bytes.

The three programs must print the same thing before anything is timed.
Timing interleaves every program in a shuffled order each round and keeps
the minimum, as CLAUDE.md says for startup measurements. A difference below
about 3% is noise.

## Serve: `bench/serve/<NN-slug>/`

- `server.wand`, `server.py`, `server_async.py` — the same server three
  ways: `HTTP.serve!`, the Python standard library's `ThreadingHTTPServer`,
  and Python `asyncio` with a small HTTP/1.1 parser of its own, so that wand
  is not compared only with the slowest Python server. Each takes one
  argument, the port. bash cannot serve, so it has no column.
- `request.json` — the request to send: `method`, `path`, and `body`, which
  names a file in the task's directory (`body.json` for `02-sum`).
- `answer.txt` — the body every server must answer with.

The three tasks are `GET /health` (the connection and dispatch path),
`POST /sum` with a JSON body of 50 numbers (decode, add, encode) and
`GET /lines/1000` (about 48KB of response).

The runner picks a free port for each run, starts the server, and waits
until it answers. Before any timing, each server must give the task's answer
to its request and 404 to an unknown path. The load is `ab` (ApacheBench,
which macOS has and most Linux images can install), so one client measures
every server. Each request has a connection of its own: no server here keeps
one open. A run is `REQUESTS` requests (default 5000) at each level of
`CONNECTIONS` (default `1,16,64`) at once.

Each round runs every task, server and level in a shuffled order, with a new
server process for each run. The report keeps the round with the most
requests per second, with its p50 and p99 latency, and sums the failed and
non-2xx requests of all rounds. A run that `ab` does not finish, because a
request had no answer in 30 seconds, is counted as stopped and has no
numbers. `ab`, the servers and the runner share the
machine, so the numbers are for a comparison on one machine, not a capacity.

## Authoring: `bench/author/<NN-slug>/`

- `spec.md` — what the agent sees. It says how the program is run, what it
  prints and which exit codes it uses.
- `cases.json` — the hidden cases. The agent never sees them. Each case is
  `{name, args, stdin, files, stdout, code}`; only `name` and `stdout` are
  required. The program runs in an empty directory that holds `fixture/` (if
  the task has one) and the case's `files`.
- `reference.wand`, `reference.py`, `reference.sh` — a correct solution in
  each language. They prove that the task can be done in each, and that the
  cases are right. The agent never sees them.
- `fixture/` — optional input files that the agent can also see.

Each run is a headless `claude -p` in a new temporary directory, with no
user settings and with Bash allowed, so that the agent can run and test what
it writes. A wand run also gets `wand` on its PATH, and Part B of CLAUDE.md (the
syntax card and the tools) as the `CLAUDE.md` of its directory, so it is in
context from the first turn, as in a real wand repository. It does not get
the full reference: `wand d` answers questions about the library, and the
two places in Part B that point at the reference point at `wand d` instead.

The runner reports, for each language: the share of runs that pass every
case, the turns on passing runs and on all runs, the escapes (runs that the
agent ended as done and that then failed a case), and the cost. It also
records each tool call the agent made, and reports how many calls of each
kind (look up a name, try an expression, typecheck, run the program, and so
on) a run takes, so that the turns can be traced to a cause. Each run's
JSON line also keeps its `steps`: every command, write and edit in full, and
the whole text of each error, so that an error can be read beside the code
that caused it.

## Review: `bench/review/<NN-slug>/`

- `intent.md` — what the author of the change says it does.
- `wand/before.wand`, `wand/after.wand`, `python/before.py`,
  `python/after.py` — the same change in each language.
- `truth.json` — `{"planted": true, "effect": "..."}` when the change does
  something the intent does not cover, or `{"planted": false}`. An optional
  `"expect": ["V-SHELL4"]` names the finding codes that `wand t` reports on
  `wand/after.wand`. `bench/check.wand` holds that file to exactly those
  codes, and holds `wand/before.wand` to none.

Approximately half the items are clean. Without them, a reviewer that
blocks everything gets a perfect score. The reviewer sees only the intent
and a unified diff with neutral file names.

## What to know about the numbers

- The model has seen far more Python and bash than wand. An authoring
  result measures the language and the model's familiarity with it together.
- The cases are in a public repository, so they can reach training data.
  Change them when a result looks too good.
- Turns alone reward a fast and wrong script. Read turns beside the pass
  rate and the escapes.
- A `claude -p` run uses the account that `claude` is logged in with. On a
  subscription, the runs count toward its usage limits, and the cost column
  is the CLI's estimate at list prices, not a charge. Read it as a measure
  of size.
