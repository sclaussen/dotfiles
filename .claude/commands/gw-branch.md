# Git Branch Management Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions based on the user's request:

**Branch Actions to Execute:**
- `feature <name>`: Run `git checkout -b feature/<name>` and `git push -u origin feature/<name>`
- `bug <name>`: Run `git checkout -b bugfix/<name>` and `git push -u origin bugfix/<name>`  
- `switch <branch>`: Run `git checkout <branch>` (with fuzzy matching)
- `list`: Run `git branch -a` with status and activity info
- `delete <branch>`: Run `git branch -d <branch>` and `git push origin --delete <branch>`
- `clean`: Find merged branches and delete them after confirmation
- `sync`: Run `git fetch origin && git rebase origin/main`

**Important**: Execute the appropriate git commands immediately based on user input. Do not ask for confirmation or explain what you will do - just do it.

## Usage
```
/gw-branch help
/gw-branch <action> [options]
```

## Actions
- `help` - Show this help information

### Create Branches
- `feature <name>` - Create feature branch with intelligent naming
- `bug <name>` - Create bug fix branch with intelligent naming
- `hotfix <name>` - Create hotfix branch for production issues
- `release <version>` - Create release branch

### Branch Operations
- `switch <branch>` - Switch to existing branch (with fuzzy matching)
- `list` - List all branches with status and last activity
- `current` - Show current branch info and status
- `delete <branch>` - Safely delete branch (local and remote)
- `clean` - Clean up merged/stale branches
- `sync` - Sync current branch with remote and update from main

### Advanced Operations
- `rename <old> <new>` - Rename branch locally and remotely
- `track <remote-branch>` - Set up tracking for remote branch
- `upstream [branch]` - Set or show upstream branch

## Branch Naming Conventions

**Feature Branches:**
- `feature/user-authentication` (kebab-case)
- `feature/api-rate-limiting` 
- `feature/dashboard-redesign`

**Bug Fix Branches:**
- `bugfix/login-validation-error` (kebab-case)
- `bugfix/memory-leak-service-worker`
- `bugfix/responsive-layout-mobile`

**Hotfix Branches:**
- `hotfix/security-patch-v2.1.3`
- `hotfix/critical-data-corruption`

**Release Branches:**
- `release/v2.1.0`
- `release/2024-q1-sprint`

## Examples

### Creating Branches
```bash
/gw-branch feature user-authentication
# Creates: feature/user-authentication
# Switches to new branch
# Sets up remote tracking

/gw-branch bug login validation error
# Creates: bugfix/login-validation-error
# Handles multi-word names intelligently

/gw-branch hotfix security patch
# Creates: hotfix/security-patch
# Ready for production deployment
```

### Branch Operations
```bash
/gw-branch switch feature/dashboard
# Fuzzy matches and switches to feature/dashboard-redesign

/gw-branch list
# Shows:
# * main (up-to-date with origin/main)
# * feature/user-auth (2 commits ahead, 1 day old)
#   bugfix/login-fix (merged, safe to delete)

/gw-branch clean
# Removes merged branches
# Prompts for confirmation on unmerged branches

/gw-branch sync
# Fetches latest from remote
# Merges main into current feature branch
# Resolves conflicts if any
```

## Intelligent Features

### Smart Naming
- Converts spaces to hyphens automatically
- Removes special characters
- Handles common abbreviations (auth, API, UI, etc.)
- Prevents duplicate branch names

### Branch Status Detection
- Shows commit count vs main/remote
- Identifies merged branches
- Detects stale branches (>30 days old)
- Shows last activity and author

### Workflow Integration
- Automatically fetches from remote before operations
- Sets up branch tracking on creation
- Integrates with existing gw-* commands
- Maintains branch history and metadata

### Safety Features
- Prevents deletion of unmerged branches without confirmation
- Backs up branch references before destructive operations
- Validates branch names against naming conventions
- Warns about force-push scenarios

## Configuration

The command respects these settings:
- **Default base branch**: `main` (auto-detects main/master)
- **Branch prefix**: Configurable per type (feature/, bugfix/, hotfix/)
- **Remote name**: `origin` (configurable)
- **Cleanup policy**: Merged branches only by default

## Integration with Other Commands

Works seamlessly with:
- `/gw-gcp` - Commit and push from feature branches
- `/gw-gpr` - Create PR from current branch
- `/gw-feature` - Long-term feature development
- `/gw-analyze` - Code analysis workflows

## What it does

1. **Branch Creation**: Creates properly named branches with remote tracking
2. **Smart Switching**: Fuzzy matching for branch names
3. **Status Monitoring**: Shows branch health and activity
4. **Cleanup Management**: Removes stale and merged branches
5. **Sync Operations**: Keeps branches updated with main
6. **Safety Checks**: Prevents data loss and naming conflicts

## Workflow Examples

### Feature Development Workflow
```bash
/gw-branch feature user-dashboard    # Create and switch
# ... develop feature ...
/gw-gcp "implement user dashboard"   # Commit and push
/gw-gpr                             # Create pull request
/gw-branch clean                    # Clean up after merge
```

### Bug Fix Workflow  
```bash
/gw-branch bug responsive-layout    # Create bug fix branch
# ... fix the issue ...
/gw-gcp "fix responsive layout on mobile"
/gw-gpr                            # Create PR for review
```

### Maintenance Workflow
```bash
/gw-branch list                    # Review all branches
/gw-branch clean                   # Remove merged branches
/gw-branch sync                    # Update current branch
```

This command provides enterprise-grade branch management with developer-friendly workflows and intelligent automation.