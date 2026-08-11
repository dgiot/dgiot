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
