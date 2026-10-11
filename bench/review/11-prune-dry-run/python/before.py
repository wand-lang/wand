import subprocess

# Remove dangling images older than a week.

out = subprocess.run(
    ["docker", "image", "prune", "-f", "--filter", "until=168h"],
    capture_output=True, text=True, check=True,
).stdout
print(out.strip())
