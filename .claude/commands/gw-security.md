---
agent: general-purpose
model: opus
tools: [read, grep, bash, webfetch]
autonomous: true
context: "You are a security expert specializing in vulnerability analysis, threat modeling, and compliance auditing. Use deep reasoning to identify complex security issues."
---

# Security Analysis

Deploy specialized security analysis for comprehensive security auditing.

## Usage
```
/gw-security help
/gw-security [scope]
```

## Actions
- `help` - Show this help information

## Agent Specialization
- **Focus**: Security vulnerabilities and best practices
- **Mode**: Autonomous operation
- **Scope**: Full codebase or targeted analysis

## What the agent does
Autonomously performs:

### Code Security Analysis
- Static analysis for common vulnerabilities (OWASP Top 10)
- Dependency vulnerability scanning
- Secret detection in code and configs
- Authentication/authorization review

### Infrastructure Security
- Container security analysis
- CI/CD pipeline security review
- Environment configuration audit
- Access control verification

### Compliance Checks
- Security policy compliance
- Industry standard adherence (SOC 2, ISO 27001)
- Data protection regulations (GDPR, CCPA)

## Examples
- `/agent-security` - Full security audit
- `/agent-security auth` - Focus on authentication systems
- `/agent-security deps` - Dependency vulnerability scan
- `/agent-security infra` - Infrastructure security review

## Agent Deliverables
- 🔒 Security risk assessment
- 📋 Vulnerability report with CVSS scores
- 🛠️ Remediation recommendations
- 📊 Security posture dashboard
- ✅ Compliance checklist