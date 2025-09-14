- remember when editing the zshrc file that it needs to support both debian and apple os
- IMPORTANT: Never edit files in the .claude/ directory directly. Always edit files in dotfiles/claude/ first, then run ./home and ./repo to deploy them
- Make sure the fix is first in dotfiles/claude, then I will run ./home and ./repo to install those, don't make changes directly to .claude/ dir

## Deployment Scripts

- **./home** - Copies dotfiles to home directory (zshrc, emacs, claude, devcontainer configs)
  - Output format: `<target>: <source> [(archived to <archive_path>)]`
  - Example: `~/.zshrc: ~/src/dotfiles/sh/zshrc`
  - Automatically archives existing files to ~/.archive/ before overwriting

- **./repo** - Copies configuration files to current repository
  - `./repo` - Install both Claude and devcontainer configurations
  - `./repo -c` - Install Claude configuration only (.claude/)
  - `./repo -d` - Install devcontainer configuration only (.devcontainer/)
  - Output format: `<target>: <source> [(archived to .archive/<filename>.<timestamp>)]`
  - Example: `.claude/settings.json: ~/src/dotfiles/claude/settings.json`
  - Automatically archives existing files to .archive/ before overwriting
  - Adds .archive to .gitignore if not present

# Claude Notification System

The notification system consists of multiple components that work together:

## Components:

1. **notify.py** - Main notification player
   - Called by Claude's Stop hook when work finishes
   - Reads configuration from notify.yaml
   - Plays the selected sound or text-to-speech on macOS

2. **notifyUpdate.py** - Settings updater
   - Called by the /notify command
   - Updates notify.yaml with new sound settings
   - Shows usage information when called without arguments

3. **notify.yaml** - Configuration file
   - Stores current sound_setting (0-12, say_ready, say_done, or custom)
   - Stores custom_text for custom text-to-speech

4. **settings.json** - Hook configuration
   - Configures Stop hook to run notify.py when Claude finishes work

5. **notify.md** - Command definition
   - Defines the /notify command that calls notifyUpdate.py

## How it works:
- User runs `/notify [option]` to change notification sound
- notifyUpdate.py updates notify.yaml with the new setting
- When Claude finishes work, the Stop hook runs notify.py
- notify.py reads notify.yaml and plays the configured sound