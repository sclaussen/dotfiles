---
model: sonnet
---

# Code Formatting Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions:

1. **Detect formatting tools**:
   - Check for `prettier` config (`.prettierrc.*`, `package.json`)
   - Check for `black` config (`pyproject.toml`, `setup.cfg`)
   - Check for `gofmt` (Go projects)
   - Check for `rustfmt` (`.rustfmt.toml`, `Cargo.toml`)
   - Check for `clang-format` (`.clang-format`)

2. **Determine target**:
   - If user provides path: format specific files/directories
   - If no path: format entire project (git-tracked files only)
   - Respect ignore files (`.prettierignore`, etc.)

3. **Execute formatting commands**:
   - Prettier: `npx prettier --write <path>`
   - Black: `black <path>`
   - gofmt: `gofmt -w <path>`
   - rustfmt: `cargo fmt`
   - clang-format: `clang-format -i <files>`

4. **Report results**:
   - Show files that were formatted
   - Display any errors or warnings
   - Confirm formatting completion

## User Input Handling
- If user provides path/pattern: format specific targets
- If no input: format entire project
- Always use project configuration files when available

**Important**: Execute formatting immediately when this command is invoked. Do not ask for confirmation or explain what you will do - just format the code.

## Usage
```
/gw-format help
/gw-format [path]
```

## Actions
- `help` - Show this help information

## What it does
1. Detects project formatting tools and configuration
2. Applies consistent formatting to code files
3. Respects project-specific formatting rules
4. Reports all changes made

## Examples
- `/format` - Format entire project
- `/format src/` - Format specific directory
- `/format *.js` - Format files matching pattern

## Formatting Tools
- **Prettier**: JavaScript, TypeScript, JSON, CSS, Markdown
- **Black**: Python code formatting
- **gofmt**: Go code formatting
- **rustfmt**: Rust code formatting
- **clang-format**: C/C++ code formatting

## Configuration
Automatically uses project configuration files:
- `.prettierrc`, `prettier.config.js`
- `pyproject.toml`, `setup.cfg`
- `.rustfmt.toml`
- `.clang-format`

## Safety
- Creates backup of changes if requested
- Only formats files tracked by git
- Respects `.gitignore` and formatting ignore files