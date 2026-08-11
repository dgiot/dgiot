<<<<<<< HEAD
# Contributing to DGIOT

First off — thank you! 🎉 We're an open-source project powered by contributors from around the world.

## Ways to Contribute

| What | Good For | Time |
|------|----------|------|
| **Report a bug** | Found something broken? | 5 min |
| **Translate docs** | Speak a language other than Chinese? | 30 min |
| **Add a protocol adapter** | Know a protocol we don't support? | 2-4 hrs |
| **Fix a good first issue** | New to the codebase? | 1-2 hrs |
| **Write a plugin** | Built something useful on DGIOT? | Flexible |
| **Improve docs** | Found unclear documentation? | 15 min |

## Development Setup

```bash
# Prerequisites
git clone https://github.com/dgiot/dgiot.git
cd dgiot

# Backend (Erlang/OTP)
cd apps/dgiot
rebar3 compile

# Frontend (Vue 3)
cd apps/dgiot_web
npm install && npm run dev

# Full stack with Docker
docker-compose -f docker/docker-compose.yml up -d
```

## Project Structure

```
dgiot/
├── apps/
│   ├── dgiot/           # Core Erlang/OTP application
│   ├── dgiot_web/       # Vue 3 frontend
│   ├── dgiot_mqtt/      # MQTT broker
│   └── dgiot_rule/      # Rules engine
├── protocols/           # 300+ protocol adapters
├── plugins/             # Community plugins
├── docker/              # Docker deployment
└── docs/                # Documentation
```

## Protocol Adapter Guide

Adding a new protocol is the most impactful contribution. Template:

```erlang
-module(dgiot_protocol_myproto).
-behaviour(dgiot_protocol).

%% Required callbacks
-export([connect/1, disconnect/1, read/2, write/3]).

connect(Args) -> {ok, Connection}.
disconnect(Connection) -> ok.
read(Connection, Address) -> {ok, Value}.
write(Connection, Address, Value) -> ok.
```

See [existing adapters](https://github.com/dgiot/dgiot/tree/main/protocols) for real examples.

## Pull Request Process

1. **Fork** the repo
2. **Branch**: `feature/your-feature` or `fix/your-bug`
3. **Code**: Follow existing patterns, add comments in English
4. **Test**: Run `make test` to verify nothing breaks
5. **Commit**: Use [conventional commits](https://www.conventionalcommits.org/)
6. **PR**: Describe what + why + how to test
7. **Review**: Maintainers respond within 48 hours

## Code of Conduct

Be respectful. Be constructive. We follow the [Contributor Covenant](https://www.contributor-covenant.org/).

## Questions?

- [GitHub Discussions](https://github.com/dgiot/dgiot/discussions) — English
- [Discord](https://discord.gg/xxx) — Chat
- [Gitee Issues](https://gitee.com/dgiot/dgiot/issues) — 中文

---

*DGIOT is Apache 2.0 licensed. Contributions are also licensed under Apache 2.0.*
=======
# Contributing

You are welcome to submit any bugs, issues and feature requests on this repository.


## Commit Message Guidelines

We have very precise rules over how our git commit messages can be formatted. This leads to **more readable messages** that are easy to follow when looking through the **project history**.

### Commit Message Format

Each commit message consists of a **header**, a **body** and a **footer**. The header has a special format that includes a **type**, a **scope** and a **subject**:

```
<type>(<scope>): <subject>
<BLANK LINE>
<body>
<BLANK LINE>
<footer>
```



The **header** with **type** is mandatory. The **scope** of the header is optional. This repository has no predefined scopes. A custom scope can be used for clarity if desired.

Any line of the commit message cannot be longer 100 characters! This allows the message to be easier to read on GitHub as well as in various git tools.

The footer should contain a [closing reference to an issue](https://help.github.com/articles/closing-issues-via-commit-messages/) if any.

Example 1:

```
feat: add Fuji release compose files
```

```
fix(script): correct run script to use the right ports

Previously device services used wrong port numbers. This commit fixes the port numbers to use the latest port numbers.

Closes: #123, #245, #992
```

### Revert

If the commit reverts a previous commit, it should begin with `revert: `, followed by the header of the reverted commit. In the body it should say: `This reverts commit <hash>.`, where the hash is the SHA of the commit being reverted.

### Type

Must be one of the following:

- **feat**: New feature for the user, not a new feature for build script
- **fix**: Bug fix for the user, not a fix to a build script
- **docs**: Documentation only changes
- **style**: Formatting, missing semi colons, etc; no production code change
- **refactor**: Refactoring production code, eg. renaming a variable
- **chore**: Updating grunt tasks etc; no production code change
- **perf**: A code change that improves performance
- **test**: Adding missing tests, refactoring tests; no production code change
- **build**: Changes that affect the CI/CD pipeline or build system or external dependencies (example scopes: travis, jenkins, makefile)
- **ci**: Changes provided by DevOps for CI purposes.
- **revert**: Reverts a previous commit.

+ commit 常用 type
+ type	含义
+ feat	新功能
+ fix	修复 bug
+ docs	修改文档
+ style	代码格式修改
+ refactor	重构（即不是新增功能，也不是修复 bug）
+ perf	更改代码以提高性能
+ test	增加测试
+ build	构建过程或辅助工具的变动
+ ci	修改项目持续集成流程
+ chore	其他类型的提交
+ revert	恢复上一次提交

### Scope

There are no predefined scopes for this repository. A custom scope can be provided for clarity.

### Subject

The subject contains a succinct description of the change:

- use the imperative, present tense: "change" not "changed" nor "changes"
- don't capitalize the first letter
- no dot (.) at the end

### Body

Just as in the **subject**, use the imperative, present tense: "change" not "changed" nor "changes". The body should include the motivation for the change and contrast this with previous behavior.

### Footer

The footer should contain any information about **Breaking Changes** and is also the place to reference GitHub issues that this commit **Closes**.

**Breaking Changes** should start with the word `BREAKING CHANGE:` with a space or two newlines. The rest of the commit message is then used for this.
>>>>>>> origin/dgaiot-plugins
