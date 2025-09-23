---
model: sonnet
---

# Project Cleanup Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions:

1. **Detect project type** and cleanup targets:
   - Node.js: `node_modules/.cache`, `dist/`, `build/`, `.next/`
   - Python: `__pycache__/`, `.pytest_cache/`, `*.pyc`, `.venv/`
   - Go: Go build cache, binaries
   - Rust: `target/` directory
   - Java: `target/`, `.class` files

2. **Execute cleanup commands**:
   - Remove build artifacts: `rm -rf dist/ build/ .next/`
   - Clear caches: `npm cache clean --force` or equivalent
   - Remove temp files: `find . -name "*.log" -delete`
   - Optional: Remove dependencies if user specifies deep clean

3. **Report cleanup results**:
   - Show freed disk space
   - List removed directories/files
   - Confirm project is clean

**Important**: Execute cleanup immediately when this command is invoked. Do not ask for confirmation unless removing dependencies.

## Usage
```
/gw-clean help
/gw-clean [level]
```

## Actions
- `help` - Show this help information

## What it does
1. Removes build artifacts and generated files
2. Clears dependency caches
3. Resets temporary files and logs
4. Optionally removes node_modules, .venv, etc.

## Examples
- `/clean` - Standard cleanup (build files, caches)
- `/clean deep` - Deep clean including dependencies
- `/clean cache` - Clear caches only

## Cleanup Levels
- **Standard**: Build outputs, logs, temp files
- **Deep**: + node_modules, .venv, target/, etc.
- **Cache**: Package manager and build caches only

## Files Removed
- **Build**: `dist/`, `build/`, `target/`, `__pycache__/`
- **Dependencies**: `node_modules/`, `.venv/`, `vendor/`
- **Caches**: `.npm/`, `.yarn/`, `pip cache`, cargo cache
- **Logs**: `*.log`, `.log/`, crash reports
- **Temp**: `.tmp/`, OS temp files

## Safety
- Shows what will be removed before deletion
- Preserves source code and configuration
- Respects `.gitignore` patterns