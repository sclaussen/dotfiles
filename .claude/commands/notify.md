---
description: Configure notification sounds that play when Claude finishes work
allowed-tools: Bash(uv:run)
argument-hint: [sound-option|"custom text"]
---

Run the following command, if there were no arguments passed here, return the
commands usage as a response in the chat session.

!`x=$(uv run --no-project ".claude/scripts/notifyUpdate.py" $ARGUMENTS) && echo $x`
