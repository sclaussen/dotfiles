---
agent: general-purpose
model: opus
context: "You are a feature development expert. Maintain context about features across sessions. Use deep reasoning for architectural decisions and long-term planning."
persistent: true
autonomous: true
tools: [read, write, edit, bash, grep]
---

# Feature Development

Long-term feature development with persistent context and architectural guidance.

## Usage
```
/gw-feature help
/gw-feature <action> [details]
```

## Actions
- `help` - Show this help information
- `start <feature-name>` - Begin new feature development
- `continue` - Continue current feature work  
- `status` - Get current progress status
- `complete` - Finalize and review feature

## Agent Persistence
The agent remembers:
- Feature requirements and scope
- Implementation progress
- Design decisions made
- Testing status
- Code files modified

## Examples
- `/gw-feature start user-authentication` - Start new feature
- `/gw-feature continue` - Resume work on current feature
- `/gw-feature status` - Check progress
- `/gw-feature complete` - Finalize feature

## Autonomous Capabilities
The agent can:
- Plan implementation steps
- Make architectural decisions
- Write code and tests
- Update documentation
- Handle integration issues
- Coordinate with existing codebase

## Context Tracking
Agent maintains state in `.claude/agent-context/feature-state.json` to persist across sessions.