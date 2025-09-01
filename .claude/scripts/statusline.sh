#!/bin/bash

# Claude Code Status Line Script  
# Colorful format: [$model] $user:$pwd ($branch)

# Read input from stdin
input=$(cat)

# Extract current directory from JSON input, fallback to pwd
if command -v jq >/dev/null 2>&1; then
    current_dir=$(echo "$input" | jq -r '.workspace.current_dir // empty' 2>/dev/null)
fi

# Fallback to pwd if jq is not available or extraction failed
if [[ -z "$current_dir" ]]; then
    current_dir=$(pwd)
fi

# Convert absolute path to relative with ~ for home directory
display_path="${current_dir/#$HOME/~}"

# Abbreviate long paths (keep first and last 2 directories)
if [[ $(echo "$display_path" | tr '/' '\n' | wc -l) -gt 4 ]]; then
    first_part=$(echo "$display_path" | cut -d'/' -f1-2)
    last_part=$(echo "$display_path" | rev | cut -d'/' -f1-2 | rev)
    display_path="${first_part}/.../${last_part}"
fi

# Get git status if in a git repository
git_info=""
branch_only=""
if git rev-parse --git-dir >/dev/null 2>&1; then
    # Skip git operations if index is locked to avoid hanging
    if [[ ! -f "$(git rev-parse --git-dir)/index.lock" ]]; then
        branch_name=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
        if [[ -n "$branch_name" ]]; then
            # Check for git status indicators
            git_status=""
            if ! git diff-index --quiet HEAD -- 2>/dev/null; then
                git_status="*"  # Modified files
            elif ! git diff --cached --quiet 2>/dev/null; then
                git_status="+"  # Staged files
            fi
            git_info=" git:($branch_name$git_status)"
            branch_only="$branch_name$git_status"
        fi
    fi
fi

# Get model name from JSON input
model_name=""
if command -v jq >/dev/null 2>&1; then
    model_name=$(echo "$input" | jq -r '.model.display_name // empty' 2>/dev/null)
fi

# Fallback if model name not available
if [[ -z "$model_name" ]]; then
    model_name="Claude"
fi

# Get username
username=$(whoami)

# Color definitions for vibrant output (works in both light and dark mode)
# Using bright colors that work well on both dark and light backgrounds
CYAN='\033[1;36m'      # Bright cyan for model name
GREEN='\033[1;32m'     # Bright green for username
BLUE='\033[1;34m'      # Bright blue for path
YELLOW='\033[1;33m'    # Bright yellow for git branch
MAGENTA='\033[1;35m'   # Bright magenta for brackets/separators
RESET='\033[0m'        # Reset to default color

# Output the colorful status line in required format: [$model] $user:$pwd ($branch)
if [[ -n "$branch_only" ]]; then
    # In git repo: [$model] $user:$pwd ($branch)
    printf "${MAGENTA}[${CYAN}%s${MAGENTA}] ${GREEN}%s${MAGENTA}:${BLUE}%s ${MAGENTA}(${YELLOW}%s${MAGENTA})${RESET}" \
        "$model_name" "$username" "$display_path" "$branch_only"
else
    # Not in git repo: [$model] $user:$pwd
    printf "${MAGENTA}[${CYAN}%s${MAGENTA}] ${GREEN}%s${MAGENTA}:${BLUE}%s${RESET}" \
        "$model_name" "$username" "$display_path"
fi