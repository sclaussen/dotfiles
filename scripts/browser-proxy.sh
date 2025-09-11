#!/bin/bash
# Browser proxy script for devcontainer CLI access
# This mimics VS Code's browser opening behavior

URL="$1"

# If we're in a devcontainer CLI session, use SSH to open browser on host
if [ -n "$DEVCONTAINER_CLI" ]; then
    # Try to use SSH to execute open command on host
    # This assumes SSH access back to host is configured
    ssh -o ConnectTimeout=2 -o StrictHostKeyChecking=no host.docker.internal "open '$URL'" 2>/dev/null || \
    ssh -o ConnectTimeout=2 -o StrictHostKeyChecking=no docker.for.mac.localhost "open '$URL'" 2>/dev/null || \
    ssh -o ConnectTimeout=2 -o StrictHostKeyChecking=no $(ip route show default | awk '/default/ {print $3}') "open '$URL'" 2>/dev/null || \
    echo "$URL"
elif [ -n "$CODESPACES" ]; then
    # GitHub Codespaces environment
    echo "$URL"
elif [ -n "$VSCODE_IPC_HOOK_CLI" ]; then
    # VS Code terminal - use VS Code's browser opening
    code --open-url "$URL" 2>/dev/null || echo "$URL"
else
    # Fallback - just echo the URL
    echo "$URL"
fi