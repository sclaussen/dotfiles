#!/bin/bash

# Claude Code Status Line Script
# Format: [$model] $user:$pwd ($branch)

# Read input from stdin
input=$(cat)

# Extract current directory and model from JSON input
if command -v jq >/dev/null 2>&1; then
    current_dir=$(echo "$input" | jq -r '.workspace.current_dir // empty' 2>/dev/null)
    model_display_name=$(echo "$input" | jq -r '.model.display_name // empty' 2>/dev/null)
fi

# Fallback to pwd and default model if jq is not available or extraction failed
if [[ -z "$current_dir" ]]; then
    current_dir=$(pwd)
fi
if [[ -z "$model_display_name" ]]; then
    model_display_name="Claude"
fi

# Get dynamic user
current_user=$(whoami)

# Convert absolute path to relative with ~ for home directory
display_path="${current_dir/#$HOME/~}"

# Detect macOS light/dark mode for color scheme
if [[ "$OSTYPE" == "darwin"* ]]; then
    auto=$(defaults read -g AppleInterfaceStyleSwitchesAutomatically 2>/dev/null)
    mode=$(defaults read -g AppleInterfaceStyle 2>/dev/null)
    
    if [[ ("$auto" == "1" && -z "$mode") || "$mode" == "Dark" ]]; then
        # Dark mode colors
        bracket='\033[1;37m'      # Bright white
        model_color='\033[0;36m'  # Cyan
        user_color='\033[0;33m'   # Orange
        colon_color='\033[1;37m'  # Bright white
        path_color='\033[1;37m'   # Bright white
        branch_color='\033[0;36m' # Cyan
    else
        # Light mode colors
        bracket='\033[1;30m'      # Dark gray
        model_color='\033[0;34m'  # Dark blue
        user_color='\033[0;33m'   # Dark orange
        colon_color='\033[1;30m'  # Dark gray
        path_color='\033[0;30m'   # Black
        branch_color='\033[0;34m' # Dark blue
    fi
else
    # Default colors for Linux/other systems
    bracket='\033[1;30m'      # Dark gray
    model_color='\033[0;34m'  # Dark blue
    user_color='\033[0;33m'   # Dark orange
    colon_color='\033[1;30m'  # Dark gray
    path_color='\033[0;30m'   # Black
    branch_color='\033[0;34m' # Dark blue
fi
reset='\033[0m'

# Get git branch information - only show if in git repo
branch=""
if git rev-parse --git-dir >/dev/null 2>&1; then
    # Skip git operations if index is locked to avoid hanging
    if [[ ! -f "$(git rev-parse --git-dir)/index.lock" ]]; then
        branch_name=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
        if [[ -n "$branch_name" ]]; then
            branch=" (${branch_color}${branch_name}${reset})"
        fi
    fi
fi

# Construct the status line: [$model] $user:$pwd ($branch)
printf "${bracket}[${model_color}${model_display_name}${reset}${bracket}]${reset} ${user_color}${current_user}${reset}${colon_color}:${reset}${path_color}${display_path}${reset}${branch}"