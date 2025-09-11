#!/usr/bin/env python3

import json
import os
import subprocess
import sys
from pathlib import Path

def get_statusline():
    """Generate statusline for Claude Code"""
    try:
        # Read input from stdin
        input_data = json.loads(sys.stdin.read())
        current_dir = input_data.get('workspace', {}).get('current_dir', os.getcwd())

        # Get username and hostname
        username = os.getenv('USER', 'user')
        try:
            hostname = subprocess.check_output(['hostname', '-s'], text=True).strip()
        except:
            hostname = 'localhost'

        # Get git branch if in a git repository
        git_branch = ""
        try:
            os.chdir(current_dir)

            # Check if we're in a git repository
            subprocess.check_output(['git', 'rev-parse', '--git-dir'],
                                   stderr=subprocess.DEVNULL)

            # Get branch name
            try:
                branch_name = subprocess.check_output(
                    ['git', 'symbolic-ref', '--quiet', '--short', 'HEAD'],
                    text=True, stderr=subprocess.DEVNULL
                ).strip()
            except:
                # Fallback to commit hash if no branch
                branch_name = subprocess.check_output(
                    ['git', 'rev-parse', '--short', 'HEAD'],
                    text=True, stderr=subprocess.DEVNULL
                ).strip()

            if branch_name:
                git_branch = f"(git: {branch_name})"
        except:
            # Not in a git repository or git not available
            pass

        # Format display path (replace home with ~)
        home = str(Path.home())
        display_path = current_dir.replace(home, '~', 1)

        # ANSI color codes - your custom color scheme
        green = '\033[92m'                 # Bright green - using same exact string for both
        electric_cyan = '\033[96m'         # Bright cyan for username
        gold = '\033[38;2;255;215;0m'      # Gold for directory path
        reset = '\033[0m'

        # Git branch info in zsh style - moved to front
        git_info = ""
        if git_branch:
            # Extract just the branch name and check for changes
            branch_name = git_branch.replace("(git: ", "").replace(")", "")
            try:
                # Check if there are uncommitted changes
                subprocess.check_output(['git', 'diff-index', '--quiet', 'HEAD', '--'],
                                      stderr=subprocess.DEVNULL, cwd=current_dir)
                git_info = f"{green}[{branch_name}]{reset} "
            except:
                git_info = f"{green}[{branch_name}*]{reset} "

        # Build statusline: [Opus] [branch*] user:~/path
        # Force both to use the exact same color string
        # Add reset at beginning to fix Claude Code shading bug
        statusline = f"{reset}{green}[Opus]{reset} {git_info}{electric_cyan}{username}{reset} {gold}{display_path}{reset}"

        print(statusline)

    except Exception as e:
        # Fallback statusline on error
        green = '\033[92m'
        electric_cyan = '\033[96m'
        gold = '\033[38;2;255;215;0m'
        reset = '\033[0m'
        print(f"{reset}{green}[Opus]{reset} {electric_cyan}{os.getenv('USER', 'user')}{reset} {gold}{os.getcwd()}{reset}")

if __name__ == "__main__":
    get_statusline()
