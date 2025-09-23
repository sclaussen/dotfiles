---
agent: general-purpose
model: sonnet
autonomous: true
tools: [bash, read, grep]
context: "You are a Git rebase expert. Handle the common workflow of committing current changes, pulling with rebase, and managing any conflicts that arise. Provide clear guidance for conflict resolution."
---

# Git Rebase with Auto-commit

Intelligent git rebase workflow that auto-commits current changes, pulls with rebase, and handles conflicts.

## Usage
```
/gw-grebase help
/gw-grebase [base-branch]
```

## Actions
- `help` - Show this help information

## Agent Configuration
- **Type**: General-purpose agent
- **Mode**: Fully autonomous
- **Tools**: Bash, Read, Grep
- **Behavior**: Multi-step Git workflow with conflict handling

## What the agent does
The agent autonomously:
1. Checks for uncommitted changes and auto-commits if needed
2. Identifies the base branch (main, master, develop, or specified)
3. Performs `git pull --rebase origin <base-branch>`
4. Detects and handles rebase conflicts with guidance
5. Continues rebase automatically when possible
6. Provides status and next steps

## Examples
- `/gw-grebase` - Rebase current branch onto origin/main
- `/gw-grebase develop` - Rebase current branch onto origin/develop
- `/gw-grebase master` - Rebase current branch onto origin/master

## Workflow Steps
1. **Pre-flight**: Check git status and working tree
2. **Auto-commit**: Commit any uncommitted changes with intelligent message
3. **Fetch**: Update remote references
4. **Rebase**: Pull with rebase from specified or detected base branch
5. **Conflict Resolution**: Guide through conflicts if they occur
6. **Continue**: Complete rebase when conflicts are resolved
7. **Status**: Show final state and any follow-up actions needed

## Conflict Handling
When conflicts occur, the agent:
- 🔍 **Identifies** conflicted files
- 📝 **Explains** the nature of conflicts
- 💡 **Provides** resolution strategies
- ✅ **Guides** through `git add` and `git rebase --continue`
- 🚨 **Offers** abort option if needed

## Output
The agent provides:
- 📊 **Status**: Current branch and working tree state
- ⚡ **Actions**: Commands executed and their results  
- ⚠️ **Conflicts**: Detailed conflict analysis and resolution steps
- ✅ **Success**: Final rebase status and branch state
- 🔄 **Next Steps**: Recommended follow-up actions

## Integration
Works seamlessly with:
- `/gw-branch` - Branch creation and management
- `/gw-gc` - Additional commits after rebase
- `/gw-gcp` - Commit and push after successful rebase
- `/gw-gpr` - Create PR after rebasing and syncing

## Safety Features
- Always commits work-in-progress to avoid data loss
- Provides clear abort instructions if rebase goes wrong
- Shows detailed conflict information for manual resolution
- Maintains backup references for recovery if needed