# Create GitHub Issue

Create a well-structured GitHub issue for the repository.

## Usage
```
/gw-issue help
/gw-issue <title> [description]
```

## Actions
- `help` - Show this help information

## What it does
1. Creates a GitHub issue with proper formatting
2. Adds relevant labels based on content analysis
3. Assigns to current user if desired
4. Returns issue URL for tracking

## Examples
- `/issue Bug in user login validation`
- `/issue Feature request: Add dark mode support`
- `/issue Performance issue with large datasets in dashboard component`

## Issue Template
Issues are created with:
- **Clear title** from your input
- **Detailed description** with context
- **Steps to reproduce** (for bugs)
- **Acceptance criteria** (for features)
- **Labels** automatically applied based on content
- **Claude Code attribution**

## Supported Labels
- `bug` - For defects and errors
- `enhancement` - For new features
- `documentation` - For docs improvements
- `performance` - For optimization needs
- `security` - For security-related issues