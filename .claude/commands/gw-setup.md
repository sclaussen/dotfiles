# Project Setup Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions:

1. **Analyze project structure**:
   - Check for `package.json` (Node.js)
   - Check for `requirements.txt`, `pyproject.toml` (Python)
   - Check for `go.mod` (Go)
   - Check for `Cargo.toml` (Rust)
   - Check for `pom.xml`, `build.gradle` (Java)

2. **Install dependencies**:
   - Node.js: `npm install` or `yarn install`
   - Python: `pip install -r requirements.txt` or `poetry install`
   - Go: `go mod download`
   - Rust: `cargo build`
   - Java: `mvn install` or `./gradlew build`

3. **Setup environment**:
   - Copy `.env.example` to `.env` if exists
   - Run database migrations if applicable
   - Setup git hooks if applicable
   - Run initial build/compile

4. **Report setup results**:
   - Confirm all dependencies installed
   - Show any configuration steps needed
   - Report any errors encountered

**Important**: Execute setup immediately when this command is invoked. Handle errors gracefully and report them clearly.

## Setup Steps
- **Dependencies**: Install npm/pip/cargo packages
- **Environment**: Copy `.env.example` to `.env`
- **Database**: Set up local database if required
- **Services**: Start required background services
- **Build**: Initial build and compilation
- **Verification**: Run health checks

## Project Types
- **Node.js**: npm/yarn install, environment setup
- **Python**: pip/poetry install, virtual environment
- **Go**: go mod download, build verification
- **Docker**: docker-compose up for services