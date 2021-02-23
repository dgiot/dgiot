# rebar3_neotoma_plugin
Rebar3 neotoma (Parser Expression Grammar) compiler plugin.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [rebar3_neotoma_plugin]}.

Then just call your plugin directly in an existing application:


    $ rebar3 neotoma compile
    ===> Fetching rebar3_neotoma_plugin
    ===> Compiling rebar3_neotoma_plugin

To have it invoked automatically when running `rebar3 compile` add it as a `provider_hooks`:

```
{provider_hooks, [
                 {pre, [{compile, {neotoma, compile}}]}
                 ]}.
```
