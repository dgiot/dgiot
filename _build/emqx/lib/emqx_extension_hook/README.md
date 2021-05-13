# emqx_extension_hook

The `emqx_extension_hook` extremly enhance the extensibility for EMQ X. It allow using an others programming language to mount the hooks intead of erlang.

## Feature

- [ ] Support variaty of programming language or web services.
- [x] Support all hooks of emqx.
- [x] Allows you to use the return value to extend emqx behavior.

Notes: The current version only support `python` and `java`.

We temporarily no plans to support other languages. Plaease open a issue if you have to use other programming languages.

## Architecture

```
                            EMQ X
                            +============================+
                            |        Extension           |
 +----------+    CONNECT    | Hooks +----------------+   |
 |  Client  | <===========> - - - ->|    Drivers     |   |
 +----------+    PUB/SUB    |       +----------------+   |
                            |               |            |
                            +===============|============+
                                            |
                                            | Callbacks
             Third-party Runtimes           |
             +=======================+      |
             |  Python Script/ Java  |<-----+
             |  Classes/ Others      |
             +=======================+
```

## Drivers

### Python

***Requirements:***

- It requires the emqx hosted machine has Python2/Python3 Runtimes
- An executable commands in your shell, i,g: `python2` or `python3`

***Examples:***

See `test/scripts/main.py`

### Java

***Requirements:***

- It requires the emqx hosted machine has Java 8+ Runtimes
- An executable commands in your shell, i,g: `java`

***Examples:***

See `test/scripts/Main.java`

## Configurations

| Name                | Data Type | Options                               | Default          | Description                      |
| ------------------- | --------- | ------------------------------------- | ---------------- | -------------------------------- |
| drivers             | Enum      | `python2`<br /> `python3`<br />`java` | `python3`        | Drivers type                     |
| <type>.path         | String    | -                                     | `data/extension` | The codes/library search path    |
| <type>.call_timeout | Duration  | -                                     | `5s`             | Function call timeout            |
| <type>.pool_size    | Integer   | -                                     | `8`              | The pool size for the driver     |
| <type>.init_module  | String    | -                                     | main             | The module name for initial call |

## SDK

See `sdk/README.md`

## Beachmark

TODOs

## Known Issues or TODOs

- Configurable Log System.
    * The Java driver can not redirect the `stderr` stream to erlang vm on Windows platform

## Reference

- [erlport](https://hub.fastgit.org/hdima/erlport)
- [Eexternal Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
- [The Ports Tutorial of Erlang](http://erlang.org/doc/tutorial/c_port.html)
