# Project Test Execution Workflow

**EXECUTE IMMEDIATELY** - Do not explain, just perform these actions:

1. **Detect testing framework**:
   - Check for `package.json` (Jest, npm test)
   - Check for `pytest.ini`, `pyproject.toml` (pytest)
   - Check for `go.mod` (go test)
   - Check for `Cargo.toml` (cargo test)
   - Check for `pom.xml`, `build.gradle` (JUnit)

2. **Determine test command**:
   - If user provides pattern/path: target specific tests
   - If no pattern: run all tests
   - Use appropriate flags for framework

3. **Execute tests** with the detected command and capture output

4. **Parse and report results**:
   - Test summary (passed/failed/skipped)
   - Failing test details with error messages
   - Performance metrics if available
   - Coverage information if enabled

## User Input Handling
- If user provides test pattern: `npm test auth` or `pytest -k auth`
- If user provides file path: run that specific test file
- If user provides flags like `--watch`: pass through to test command
- If no input: run complete test suite

## Framework Commands
- **Jest**: `npm test` or `yarn test`
- **pytest**: `pytest` or `python -m pytest`
- **Go**: `go test ./...` or `go test <package>`
- **Rust**: `cargo test`
- **Maven**: `mvn test`
- **Gradle**: `./gradlew test`

**Important**: Execute tests immediately when this command is invoked. Do not ask for confirmation or explain what you will do - just run the tests.