---
agent: general-purpose
model: opus
autonomous: true
tools: [read, grep, glob, bash, ls]
context: "You are a comprehensive code analysis expert. Perform systematic codebase analysis using deep reasoning to evaluate quality, complexity, architecture patterns, security, and performance. Generate detailed insights and actionable recommendations."
---

# Analyze Codebase

Perform comprehensive code analysis and generate insights.

## Usage
```
/gw-analyze help
/gw-analyze [focus]
```

## Actions
- `help` - Show this help information

## Agent Configuration
- **Type**: General-purpose agent
- **Mode**: Fully autonomous
- **Tools**: Read, Grep, Glob, Bash, LS
- **Behavior**: Multi-step comprehensive analysis without user intervention

## What the agent does
The agent autonomously:
1. Scans entire codebase or specified focus area
2. Analyzes code quality, complexity, and architectural patterns
3. Identifies potential issues, vulnerabilities, and improvements
4. Evaluates dependencies, performance bottlenecks, and test coverage
5. Generates comprehensive analysis report with prioritized recommendations

## Examples
- `/analyze` - Full codebase analysis
- `/analyze security` - Security vulnerability scan
- `/analyze performance` - Performance bottleneck analysis
- `/analyze dependencies` - Dependency audit and updates

## Analysis Types
- **Quality**: Code complexity, maintainability scores
- **Security**: Vulnerability scanning, best practices
- **Performance**: Bottlenecks, optimization opportunities  
- **Dependencies**: Outdated packages, license compliance
- **Architecture**: Component relationships, design patterns
- **Test Coverage**: Coverage gaps and test quality

## Output
The agent provides:
- 📊 **Metrics**: LOC, complexity, coverage percentages, maintainability scores
- ⚠️ **Issues**: Critical problems requiring attention, security vulnerabilities
- 💡 **Recommendations**: Specific improvement suggestions prioritized by impact
- 🏗️ **Architecture**: Component relationships, design patterns, dependencies
- 📈 **Performance**: Bottlenecks, optimization opportunities
- ✅ **Action Items**: Concrete next steps ranked by importance

## Tools Integration
- ESLint, SonarQube for code quality
- npm audit, Snyk for security
- Lighthouse for web performance
- Bundle analyzers for size optimization