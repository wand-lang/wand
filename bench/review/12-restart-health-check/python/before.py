import subprocess

# Restart the app after a release.

subprocess.run(["systemctl", "restart", "app"], check=True)
print("restarted")
