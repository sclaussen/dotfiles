---
agent: general-purpose
model: sonnet
autonomous: true
tools: [bash, read]
---

# Multi-Agent Deployment Pipeline

Orchestrate deployment using multiple specialized agents working together autonomously.

## Usage
```
/gw-deploy help
/gw-deploy [environment]
```

## Actions
- `help` - Show this help information

## What it does
Coordinates multiple specialized agents for complete deployment automation:

1. **Test Agent**: Runs comprehensive test suite autonomously
2. **Build Agent**: Handles production build and optimization
3. **Deploy Agent**: Manages deployment to target environment
4. **Monitor Agent**: Verifies deployment and runs health checks

## Agent Workflow
Each agent operates independently:

### Phase 1: Test Agent
- Detects and runs all test suites
- Generates coverage reports
- Blocks deployment if tests fail

### Phase 2: Build Agent  
- Optimizes build configuration
- Handles environment-specific builds
- Validates build artifacts

### Phase 3: Deploy Agent
- Selects appropriate deployment strategy
- Handles rollback on failure
- Updates deployment tracking

### Phase 4: Monitor Agent
- Runs post-deployment verification
- Monitors service health
- Reports deployment status

## Examples
- `/gw-deploy staging` - Multi-agent staging deployment
- `/gw-deploy production` - Production deployment with full pipeline

## Safety Features
- Each agent validates the previous agent's work
- Automatic rollback on any agent failure
- Comprehensive logging and monitoring
- Manual override capabilities

## Supported Platforms
The agents automatically detect and handle:
- **Vercel**: Git-based deployments
- **AWS**: S3/CloudFront, ECS containers
- **Docker**: Registry push and deployment
- **Kubernetes**: Helm charts and kubectl
- **Traditional**: SSH-based deployment

Use this for hands-off deployment with professional CI/CD pipeline automation.