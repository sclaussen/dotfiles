- remember when editing the zshrc file that it needs to support both debian and apple os

## Deployment Scripts

- **./home** - Copies dotfiles to home directory (shell and emacs configurations only)
  - Copies: zshrc, emacs, emacs.d
  - Output format: `<target>: <source> [(archived to <archive_path>)]`
  - Example: `~/.zshrc: ~/src/dotfiles/sh/zshrc`
  - Automatically archives existing files to ~/.archive/ before overwriting
  - Preserves ~/.emacs.d/elpa directory when updating emacs configuration

## Configuration Installation

Claude and devcontainer configurations are handled by the `dev` tool. To install configurations:

```bash
# Install both Claude and devcontainer configs to current directory
dev config

# Install to specific directory
dev config ~/src/myproject

# Install Claude config only
dev config -c

# Install devcontainer config only
dev config -d
```

The `dev config` command uses templates from:
1. User customizations: `~/.dev/templates/claude/` and `~/.dev/templates/devcontainer/` (if they exist)
2. Default templates: From the dev repository's `templates/` directory

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