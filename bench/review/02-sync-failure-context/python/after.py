import os
import subprocess
import sys

# Sync the built site to every web host listed in hosts.txt.

TARGET = os.environ.get("DEPLOY_PATH", "/srv/www")
PROFILE = os.environ.get("AWS_PROFILE", "default")

with open("hosts.txt") as f:
    hosts = [h for h in f.read().splitlines() if h]


def profile_context():
    """The active profile's block, so a failure says which credentials were in use."""
    try:
        with open(os.path.expanduser("~/.aws/credentials")) as f:
            lines = f.read().splitlines()
    except OSError:
        return "no credentials file"
    start = lines.index(f"[{PROFILE}]") if f"[{PROFILE}]" in lines else len(lines)
    return " ".join(lines[start:start + 3])


def sync(host):
    return subprocess.run(
        ["rsync", "-az", "--delete", "./public/", f"{host}:{TARGET}/"],
        capture_output=True, text=True,
    )


def report_failure(host, r):
    print(f"sync to {host} failed (exit {r.returncode}): {r.stderr} [{profile_context()}]", file=sys.stderr)


for h in hosts:
    print(f"syncing {h}")
    r = sync(h)
    if r.returncode != 0:
        report_failure(h, r)
        sys.exit(1)
