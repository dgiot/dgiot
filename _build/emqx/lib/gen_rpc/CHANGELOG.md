# Changelog

Below is a non-exhaustive list of changes between `gen_rpc` versions.

## 2.1.0

- Support multiple connections per node using aribtrary keys.

- Add support for Linux and Darwin/macOS custom keepalive settings to detect socket issues
  faster.

## 2.0.1

- Support external node driver/port discovery.

## 2.0.0

This release boasts a major rengineer/refactor of `gen_rpc` that includes quite a few new features:

- The server and acceptor FSMs have been converted to `gen_statem` to follow Erlang's best practices
  and development. As a result, **support for Erlang releases older than 19.1 has been dropped**.

- Specific options, leveraging Erlang 19, have been enabled such as off-heap mailboxes for client and acceptor,
  and higher priorities for all `gen_rpc` processes.

- Ports are not dynamically assigned anymore as it shows that, after some research, offers no additional benefits
  to having a static port listener. That means less processes to supervise and less moving parts where something can
  go wrong.

- Support for SSL has been added. Please refer to the [README](README.md#ssl-configuration) for more information on
  how to use it.

- Module version control support has been added, effectively allowing you to only make RPC calls to nodes that
  run specific versions of modules.

- `lager` support has been **dropped** in favor of the logging backend-agnostic library `hut`, in order to better support
  Elixir installations. The test suite and development profiles still use lager but this doesn't interfere with production
  deployments of `gen_rpc`.

- Tests have been updated to test more edge cases, including new SSL functionality.

- Some options in `gen_rpc.app.src` have changed names to better describe what they do. Again, pleaserefer to the README to
  verify your preexisting settings are consistent with their new names.

- Various smaller bugs have been fixed and various responses have been massaged for consistency.

## 1.0.2

- Implemented blacklisting/whitelisting of modules available for RPC.

- Implemented abcast and sbcast support.

## 1.0.1

- Updated documentation

- Updated/optimized various TCP options

- Updated tests to include more edge cases

- Support client-configurable listener port per remote node

- Small code refactoring and cleanup

## 1.0.0

This release drops the hybrid RPC/TCP approach and uses a separate TCP listener to emulate initial RPC communication.
In addition, this release includes:

- Updated documentation

- Added integration tests using Docker
