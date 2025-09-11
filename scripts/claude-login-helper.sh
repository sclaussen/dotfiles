#!/bin/bash
# Helper script to handle Claude login from CLI devcontainer sessions
# This script captures the login URL and provides instructions

claude_login_output=$(claude /login 2>&1)

# Check if the output contains a URL
if echo "$claude_login_output" | grep -q "https://claude.ai/oauth/authorize"; then
    # Extract the URL
    login_url=$(echo "$claude_login_output" | grep -o "https://claude.ai/oauth/authorize[^[:space:]]*")
    
    echo "================================================================"
    echo "Claude Code Login"
    echo "================================================================"
    echo ""
    echo "Please open the following URL in your browser on the host machine:"
    echo ""
    echo "$login_url"
    echo ""
    echo "================================================================"
    echo ""
    echo "After logging in, paste the authorization code when prompted below."
    echo ""
    
    # Handle the authorization code input
    read -p "Paste code here if prompted > " auth_code
    if [ ! -z "$auth_code" ]; then
        echo "$auth_code" | claude
    fi
else
    # If no URL found, just display the output as-is
    echo "$claude_login_output"
fi