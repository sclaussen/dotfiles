---
agent: general-purpose
model: sonnet
context: "You are a commit specialist. Use current repository context, staged/unstaged changes, recent commit history, and project conventions to create perfect commits autonomously. Analyze the actual code changes to understand the intent and impact."
tools: [read, bash, grep]
autonomous: true
---

# Commit Changes

Create commits using a specialized agent that autonomously analyzes your current changes and generates quality commit messages.

## Usage
```
/gw-gc help
/gw-gc [description]
```

## Actions
- `help` - Show this help information

## What it does
The agent will autonomously:
- Analyze all staged and unstaged changes in your current repository
- Review recent commit history for style consistency
- Understand the code changes and their impact
- Generate conventional commit messages following project patterns
- Create commits with proper attribution
- Handle any errors or conflicts independently

## Agent Context Awareness
The agent uses your current repository state:
- **Git changes**: Staged and unstaged file modifications
- **Commit history**: Recent patterns and conventional commit style
- **Code context**: Understands what the changes actually do
- **Project patterns**: Follows your existing commit conventions
- **File analysis**: Reads changed files to understand intent

## Examples
- `/gw-gc` - Full autonomous commit based on current changes
- `/gw-gc implement user dashboard` - Agent commit with additional context

The agent generates commits following this format:
```
type(scope): description

- Detailed explanation based on actual code changes
- Why the changes were made (inferred from context)
- Any breaking changes or notable impacts

🤖 Generated with Claude Code
Co-Authored-By: Claude <noreply@anthropic.com>
```

Use this when you want Claude to handle the entire commit workflow autonomously using your current repository context.