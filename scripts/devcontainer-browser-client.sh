#!/bin/bash
# Browser client for devcontainers - sends open requests to host server

URL="$1"

# Try to connect to the browser forwarding server on the host
# Try different host addresses that might work
for HOST in host.docker.internal docker.for.mac.localhost localhost 172.17.0.1; do
    if echo "OPEN:$URL" | nc -w 1 $HOST 9559 2>/dev/null | grep -q "OK"; then
        exit 0
    fi
done

# If server isn't running or reachable, just echo the URL
echo "$URL"