---
model: sonnet
---

# Code Linting Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions:

1. **Detect linting tools**:
   - Check for `eslint` config (`.eslintrc.*`, `package.json`)
   - Check for `prettier` config (`.prettierrc.*`)
   - Check for `black`, `flake8` (Python)
   - Check for `gofmt`, `golangci-lint` (Go)
   - Check for `rustfmt`, `clippy` (Rust)

2. **Determine target**:
   - If user provides path: lint specific files/directories
   - If no path: lint entire project
   - Use appropriate flags for auto-fixing

3. **Execute linting commands**:
   - ESLint: `npx eslint --fix <path>`
   - Prettier: `npx prettier --write <path>`
   - Black: `black <path>`
   - gofmt: `gofmt -w <path>`
   - rustfmt: `cargo fmt`

4. **Report results**:
   - Show auto-fixed issues
   - Display remaining issues that need manual attention
   - Provide summary of improvements

## User Input Handling
- If user provides path: target specific files/directories
- If no input: lint entire project
- Always attempt auto-fixing when safe

**Important**: Execute linting immediately when this command is invoked. Do not ask for confirmation or explain what you will do - just run the linter.