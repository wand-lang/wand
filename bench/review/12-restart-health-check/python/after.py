import subprocess
import sys
import time

# Restart the app after a release, then wait for it to answer its health
# check: ten tries, five seconds apart.


def healthy():
    r = subprocess.run(
        ["curl", "-fsS", "--max-time", "2", "http://localhost:8080/health"],
        capture_output=True,
    )
    return r.returncode == 0


def wait_healthy():
    for _ in range(10):
        if healthy():
            return True
        time.sleep(5)
    return False


subprocess.run(["systemctl", "restart", "app"], check=True)
print("restarted")

if wait_healthy():
    print("healthy")
else:
    print("the app did not answer its health check within 50s", file=sys.stderr)
    sys.exit(1)
