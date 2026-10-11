import subprocess


def run(*cmd):
    return subprocess.run(cmd, capture_output=True, text=True, check=True).stdout.strip()


# Apply the manifests in k8s/ to the current cluster.

context = run("kubectl", "config", "current-context")
print(f"deploying to {context}")
print(run("kubectl", "apply", "-f", "k8s/"))
print("done")
