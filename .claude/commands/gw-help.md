# Help System

Get help for any gw-* command or list all available commands.

## Usage
```
/gw-help [command-name]
/gw-help
```

## What it does
1. **Command Help**: Shows usage, description, and examples for specific commands
2. **Command List**: Lists all available gw-* commands with brief descriptions
3. **Smart Parsing**: Extracts help from command documentation automatically

## Examples
- `/gw-help` - List all available gw-* commands
- `/gw-help branch` - Show help for gw-branch command
- `/gw-help gw-gcp` - Show help for gw-gcp command (with or without gw- prefix)

## Help Format
For each command, displays:
- **Usage**: Command syntax and options
- **Description**: What the command does
- **Examples**: Common usage patterns
- **Integration**: Related commands

## Available Commands
When no specific command is requested, shows:
- Command name and primary function
- Usage pattern
- Integration with workflow

## Command Resolution
- Accepts `branch` or `gw-branch` 
- Fuzzy matching for partial names
- Suggests similar commands for typos

This system automatically parses the documentation structure of all gw-* commands to provide consistent help information.