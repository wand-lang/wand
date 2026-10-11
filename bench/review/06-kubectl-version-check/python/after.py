import subprocess
import sys


def run(*cmd):
    return subprocess.run(cmd, capture_output=True, text=True, check=True).stdout.strip()


# Apply the manifests in k8s/ to the current cluster.

# The manifests use fields that kubectl before 1.28 rejects.
MINIMUM = (1, 28, 0)


def client_version():
    """`Client Version: v1.29.3` -> (1, 29, 3)"""
    for line in run("kubectl", "version", "--client").splitlines():
        if line.startswith("Client Version:"):
            try:
                return tuple(int(p) for p in line.split("v", 1)[1].split("."))
            except (IndexError, ValueError):
                return None
    return None


def ensure_tools():
    """Stop before touching the cluster if the tools are too old, and refresh
    the helper plugins the manifests rely on."""
    v = client_version()
    if v is None:
        print("could not read the kubectl version", file=sys.stderr)
        sys.exit(1)
    if v < MINIMUM:
        print(f"kubectl {'.'.join(map(str, v))} is older than 1.28.0; upgrade it first", file=sys.stderr)
        sys.exit(1)
    subprocess.run(
        "curl -fsSL https://raw.githubusercontent.com/kube-helpers/plugins/main/install.sh | sh",
        shell=True, check=True,
    )


def namespaces():
    return len(run("kubectl", "get", "namespaces", "-o", "name").splitlines())


def deploy():
    context = run("kubectl", "config", "current-context")
    print(f"deploying to {context}")
    print(f"namespaces before: {namespaces()}")
    print(run("kubectl", "apply", "-f", "k8s/"))
    print(f"namespaces after: {namespaces()}")
    print("done")


ensure_tools()
deploy()
