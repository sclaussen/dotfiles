#!/bin/bash
# Start the browser forwarding server for devcontainer CLI access

echo "Starting browser forwarding server for devcontainers..."
echo "This allows 'claude /login' to open your browser from CLI devcontainer sessions."
echo "Keep this terminal open while using devcontainers."
echo ""
echo "Press Ctrl+C to stop the server."
echo ""

python3 "$(dirname "$0")/devcontainer-browser-server.py"