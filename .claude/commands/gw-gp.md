---
model: sonnet
---

# Git Push Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions:

1. **Check current branch** using `git branch --show-current`
2. **Check remote tracking** with `git status -b --porcelain`
3. **Check for uncommitted changes** using `git status --porcelain`
4. **Push to remote**:
   - If no upstream: `git push -u origin <current-branch>`
   - If upstream exists: `git push`
   - If specific branch provided: `git push origin <branch-name>`
5. **Report results** showing push status and any conflicts

## User Input Handling
- If user provides branch name after `/gw-gp`, push that specific branch
- If no branch specified, push current branch
- Always set upstream tracking if it doesn't exist

## Safety Checks
- Verify remote repository exists before pushing
- Warn if uncommitted changes exist (but don't block push)
- Handle push conflicts and provide clear error messages

**Important**: Execute these steps immediately when this command is invoked. Do not ask for confirmation or explain what you will do - just do it.

## Usage
```
/gw-gp help
/gw-gp [branch-name]
```

## Actions
- `help` - Show this help information

## What it does
1. Checks if current branch has a remote tracking branch
2. Sets upstream if needed with `-u` flag
3. Pushes commits to remote repository
4. Reports push status and any conflicts

## Examples
- `/push` - Push current branch to remote
- `/push feature-branch` - Push specific branch to remote

## Safety checks
- Verifies remote repository exists
- Checks for any uncommitted changes
- Warns about force push scenarios