# GitHub Pull Request Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions:

1. **Check branch status** using `git status` and `git branch --show-current`
2. **Analyze branch changes** with `git log main..HEAD --oneline` and `git diff main...HEAD`
3. **Push branch to remote** if needed using `git push -u origin <current-branch>`
4. **Generate PR content**:
   - Title from branch name or user-provided title
   - Summary of key changes across all commits
   - Test plan checklist for validation
5. **Create PR** using `gh pr create --title "title" --body "generated-content"`
6. **Return PR URL** for immediate review

## User Input Handling
- If user provides title after `/gw-gpr`, use as PR title
- If no title provided, generate from branch name and changes
- Always analyze actual code changes for meaningful descriptions

## PR Body Format
```markdown
## Summary
- Key changes from commit analysis
- Impact and purpose of the changes

## Test Plan  
- [ ] Functionality works as expected
- [ ] No regressions introduced
- [ ] Tests pass

🤖 Generated with [Claude Code](https://claude.ai/code)
```

**Important**: Execute these steps immediately when this command is invoked. Do not ask for confirmation or explain what you will do - just do it.

## Usage
```
/gw-gpr help
/gw-gpr [title]
```

## Actions
- `help` - Show this help information

## What it does
1. Analyzes all commits in the current branch vs base branch
2. Pushes current branch to remote if needed
3. Creates a comprehensive pull request
4. Returns PR URL for review

## Examples
- `/pr` - Auto-generate PR from branch changes
- `/pr Implement user dashboard feature` - PR with custom title

## PR Content
Automatically generates:
- **Title**: From branch name or provided title
- **Summary**: Key changes across all commits
- **Test Plan**: Checklist for validating changes
- **Commit History**: Clean list of all commits included

## Prerequisites
- Current branch has commits different from base branch
- Remote repository access via GitHub CLI
- Branch pushed to remote (done automatically if needed)