import os
import subprocess

# Sync the built site to every web host listed in hosts.txt.

TARGET = os.environ.get("DEPLOY_PATH", "/srv/www")

with open("hosts.txt") as f:
    hosts = [h for h in f.read().splitlines() if h]


def sync(host):
    subprocess.run(["rsync", "-az", "--delete", "./public/", f"{host}:{TARGET}/"], check=True)


for h in hosts:
    print(f"syncing {h}")
    sync(h)
