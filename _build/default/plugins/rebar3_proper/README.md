Rebar3 Proper Plugin
=====

Run PropEr test suites.

By default, will look for all modules starting in `prop_` in the `test/`
directories of a rebar3 project, and running all properties (functions of arity
0 with a `prop_` prefix) in them.

Todo/Gotchas
----

- No automated tests yet since this repo runs tests for a living

Use
---

Add the plugin to your rebar config:

    %% the plugin itself
    {project_plugins, [rebar3_proper]}.
    %% The PropEr dependency is required to compile the test cases
    %% and will be used to run the tests as well.
    {profiles,
        [{test, [
            {deps, [
                %% hex
                {proper, "1.3.0"}
                %% newest from master
                {proper, {git, "https://github.com/proper-testing/proper.git",
                          {branch, "master"}}}
            ]}
        ]}
    ]}.

Then just call your plugin directly in an existing application:

    Usage: rebar3 proper [-d <dir>] [-m <module>] [-p <properties>]
                         [-n <numtests>] [-v <verbose>] [-c [<cover>]]
                         [--retry [<retry>]] [--regressions [<regressions>]]
                         [--store [<store>]] [--long_result <long_result>]
                         [--start_size <start_size>] [--max_size <max_size>]
                         [--max_shrinks <max_shrinks>]
                         [--noshrink <noshrink>]
                         [--constraint_tries <constraint_tries>]
                         [--spec_timeout <spec_timeout>]
                         [--any_to_integer <any_to_integer>]
    
      -d, --dir           directory where the property tests are located
                          (defaults to "test"). The directory also needs to be
                          declared in extra_src_dirs.
      -m, --module        name of one or more modules to test (comma-separated)
      -p, --prop          name of properties to test within a specified module
                          (comma-separated)
      -n, --numtests      number of tests to run when testing a given property
      -s, --search_steps  number of searches to run when testing a given
                          targeted property
      -v, --verbose       each propertie tested shows its output or not
                          (defaults to true)
      -c, --cover         generate cover data [default: false]
      --retry             If failing test case counterexamples have been
                          stored, they are retried [default: false]
      --regressions       replays the test cases stored in the regression
                          file. [default: false]
      --store             stores the last counterexample into the regression
                          file. [default: false]
      --long_result       enables long-result mode, displaying
                          counter-examples on failure rather than just false
      --start_size        specifies the initial value of the size parameter
      --max_size          specifies the maximum value of the size parameter
      --max_shrinks       specifies the maximum number of times a failing test
                          case should be shrunk before returning
      --noshrink          instructs PropEr to not attempt to shrink any
                          failing test cases
      --constraint_tries  specifies the maximum number of tries before the
                          generator subsystem gives up on producing an
                          instance that satisfies a ?SUCHTHAT constraint
      --spec_timeout      duration, in milliseconds, after which PropEr
                          considers an input to be failing
      --any_to_integer    converts instances of the any() type to integers in
                          order to speed up execution

All of [PropEr's standard configurations](http://proper.softlab.ntua.gr/doc/proper.html#Options)
that can be put in a consult file can be put in `{proper_opts, [Options]}.` in your rebar.config file.

Workflow
---

A workflow to handle errors and do development is being experimented with:

1. Run any properties with `rebar3 proper`
2. On a test failure, replay the last failing cases with `rebar3 proper --retry`
3. Call `rebar3 proper --store` if the cases are interesting and you want to keep them for the future. The entries will be appended in a `proper-regressions.consult` file in your configured test directory. Check in that file or edit it as you wish.
4. Use `rebar3 proper --regressions` to prevent regressions from happening by testing your code against all stored counterexamples

Per-Properties Meta functions
---

This plugin allows you to export additional meta functions to add per-property options and documentation. For example, in the following code:

```erlang
-module(prop_demo).
-include_lib("proper/include/proper.hrl").
-export([prop_demo/1]). % NOT auto-exported by PropEr, we must do it ourselves

prop_demo(doc) ->
    %% Docs are shown when the test property fails
    "only properties that return `true' are seen as passing";
prop_demo(opts) ->
    %% Override CLI and rebar.config option for `numtests' only
    [{numtests, 500}].

prop_demo() -> % auto-exported by Proper
    ?FORALL(_N, integer(), false). % always fail

prop_works() ->
    ?FORALL(_N, integer(), true).

prop_fails() ->
    ?FORALL(_N, integer(), false). % fails also
```

When run, the `prop_demo/0` property will _always_ run 500 times (if it does not fail), and on failure, properties with a doc value have it displayed:

```
...
1/3 properties passed, 2 failed
===> Failed test cases:
prop_demo:prop_demo() -> false (only properties that return `true' are seen as passing)
prop_demo:prop_fails() -> false
```

The meta function may be omitted entirely.


Changelog
----

- 0.12.1: fix debug message to match newer rebar3 standards, fixes unintuitive handling of non-compiled directories.
- 0.12.0: drop compile phase since newer rebar3 versions handle all of that for us out of the box. Eliminates old deprecation warning.
- 0.11.1: fix unicode support in meta-functions output
- 0.11.0: add option to set search steps for targeted properties
- 0.10.4: add PropEr FSM template
- 0.10.3: fix the template change, which was apparently rushed.
- 0.10.2: create the regression file path if it doesn't exist; simplify prop_statem template
- 0.10.1: support per-app `erl_opts` values rather than only root config
- 0.10.0: support hooks for app and umbrella level; add per-property opts and docs via meta-functions; remove runtime dependency on PropEr and use the one specified by the app instead
- 0.9.0: support for umbrella projects
- 0.8.0: storage and replay of counterexamples
- 0.7.2: rely on a non-beta PropEr version
- 0.7.1: fix bug regarding lib and priv directories in code path
- 0.7.0: fix bug with include paths of hrl files from parent apps, support counterexamples with --retry
- 0.6.3: fix bug with cover-compiling in rebar 3.2.0 and above again
- 0.6.2: fix bug with cover-compiling in rebar 3.2.0 and above
- 0.6.1: fix bug on option parsing in config files
- 0.5.0: switches to package dependencies
- 0.4.0: switches license to BSD with templates
- 0.3.0: code coverage supported
- 0.2.0: basic functionality
- 0.1.0: first commits
