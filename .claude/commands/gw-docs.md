# Generate Documentation

Generate or update project documentation automatically.

## Usage
```
/gw-docs help
/gw-docs [type]
```

## Actions
- `help` - Show this help information

## What it does
1. Analyzes codebase structure and exports
2. Generates API documentation from code
3. Updates README with current project info
4. Creates/updates code examples and usage guides

## Examples
- `/docs` - Generate all documentation
- `/docs api` - API documentation only
- `/docs readme` - Update README.md
- `/docs examples` - Generate code examples

## Documentation Types
- **API**: Function/class documentation from JSDoc/docstrings
- **README**: Project overview, setup, usage instructions
- **Examples**: Code samples and tutorials
- **Architecture**: System design and component diagrams
- **Changelog**: Release notes and version history

## Output Formats
- **Markdown**: `.md` files for GitHub/GitLab
- **HTML**: Static site generation (Docusaurus, VitePress)
- **JSON**: Machine-readable API specs (OpenAPI)
- **PDF**: Printable documentation

## Features
- Automatic table of contents
- Code syntax highlighting
- Cross-references and links
- Version compatibility notes