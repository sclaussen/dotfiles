# Commit and Push

Create a quality commit and immediately push to remote repository.

## Usage
```
/gw-gcp help
/gw-gcp [optional description]
```

## Actions
- `help` - Show this help information

## What it does
1. Analyzes all changes and generates a quality commit message
2. Creates the commit with proper attribution
3. Pushes the commit to the remote repository
4. Sets upstream tracking if needed

## Examples
- `/commit-push` - Auto-generate commit and push
- `/commit-push implement user dashboard` - Commit with description and push

## Workflow
1. **Analyze**: Reviews staged/unstaged changes and commit history
2. **Commit**: Creates conventional commit with descriptive message
3. **Push**: Pushes to remote with upstream tracking if needed
4. **Verify**: Confirms both commit and push succeeded

This combines the `/commit` and `/push` commands into a single streamlined workflow.