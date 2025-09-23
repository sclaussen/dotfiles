# Lines of Code Analysis

Intelligent lines of code analysis using Claude Sonnet with loc, scc, and tokei CLI tools.

## Usage
```
/gw-loc help
/gw-loc [options]
```

## Actions
- `help` - Show this help information

## What it does
1. Analyzes codebase using multiple LOC tools (loc, scc, tokei)
2. Provides intelligent insights using Claude Sonnet agent
3. Detects patterns and technical debt indicators
4. Generates recommendations for code organization
5. Compares results across different analysis tools

## Examples
- `/gw-loc` - Basic analysis with scc
- `/gw-loc --agent` - Run autonomous agent analysis with insights
- `/gw-loc --compare-tools` - Compare results from all three tools
- `/gw-loc --insights` - Generate AI-powered insights and recommendations
- `/gw-loc --directory src/` - Analyze specific directory
- `/gw-loc --format json` - Output results in JSON format

## Analysis Features
- **Multi-tool Support**: Uses loc (custom wrapper), scc, and tokei
- **Agent Mode**: Autonomous analysis with enhanced AI insights
- **Pattern Detection**: Identifies code organization patterns
- **Technical Debt**: Flags low comment ratios and large files
- **Architecture Notes**: Provides structural recommendations

## Output Types
- 📊 **Summary**: Files, lines, code, comments, blanks
- 🔍 **Patterns**: Average lines per file, codebase size insights  
- 💡 **Recommendations**: Code organization and documentation suggestions
- ⚠️ **Technical Debt**: Areas requiring attention
- 🏗️ **Architecture**: Structural improvement opportunities

## Tools Used
- **loc**: Custom bash wrapper around SCC with friendly options
- **scc**: Fast and accurate code counter with language detection
- **tokei**: Multi-language code statistics with detailed breakdown

## Agent Capabilities
When using `--agent` flag, the command runs as an autonomous agent that:
- Analyzes output from all tools intelligently
- Detects code quality patterns and anti-patterns
- Provides specific, actionable recommendations
- Identifies technical debt indicators
- Suggests architectural improvements

## Supported Formats
- **tabular**: Human-readable table format (default)
- **json**: Structured JSON for programmatic use
- **csv**: Comma-separated values for spreadsheet analysis