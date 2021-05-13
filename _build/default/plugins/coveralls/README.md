coveralls-erl
=============
[![Build Status](https://travis-ci.org/markusn/coveralls-erl.png?branch=master)](https://travis-ci.org/markusn/coveralls-erl)
[![Coverage Status](https://coveralls.io/repos/markusn/coveralls-erl/badge.png?branch=master)](https://coveralls.io/r/markusn/coveralls-erl?branch=master)
[![Hex.pm](https://img.shields.io/hexpm/v/coveralls.svg?style=flat)](https://hex.pm/packages/coveralls)

Erlang module to convert and send cover data to coveralls. Available as a hex package on https://hex.pm/packages/coveralls.

## Example usage: rebar3 and Travis CI
In order to use coveralls-erl + Travis CI in your project you will need to add the following lines to your
`rebar.config.script`:

```erlang
case os:getenv("TRAVIS") of
  "true" ->
    JobId   = os:getenv("TRAVIS_JOB_ID"),
    lists:keystore(coveralls_service_job_id, 1, CONFIG, {coveralls_service_job_id, JobId});
  _ ->
    CONFIG
end.
```

This will ensure that the rebar coveralls plugin will have access to the needed JobId and that the plugin is only run from Travis CI.

You will also need to add the following lines to your `rebar.config`:
```erlang
{plugins                , [coveralls]}. % use hex package
{cover_enabled          , true}.
{cover_export_enabled   , true}.
{coveralls_coverdata    , "_build/test/cover/eunit.coverdata"}. % or a string with wildcards or a list of files
{coveralls_service_name , "travis-ci"}. % use "travis-pro" when using with travis-ci.com
```
When using with travis-ci.com coveralls repo token also has to be added as `{coveralls_repo_token, "token_goes_here"}`

These changes will add `coveralls-erl` as a dependency, tell `rebar3` where to find the plugin, make sure that the coverage data is produced and exported and configure `coveralls-erl` to use this data and the service `travis-ci`.

And you send the coverdata to coveralls by issuing: `rebar3 as test coveralls send`

**Note:**
If you have dependencies specific to the test profile, or if you only add the coveralls dependency or any of its' configuration variables to the test profile you need to run coveralls using: `rebar3 as test coveralls send`

## Example: rebar3 and CircleCI
Example `rebar.config.script`:

```erlang
case {os:getenv("CIRCLECI"), os:getenv("COVERALLS_REPO_TOKEN")} of
    {"true", Token} when is_list(Token) ->
        JobId   = os:getenv("CIRCLE_BUILD_NUM"),
        CONFIG1 = lists:keystore(coveralls_service_job_id, 1, CONFIG, {coveralls_service_job_id, JobId}),
        lists:keystore(coveralls_repo_token, 1, CONFIG1, {coveralls_repo_token, Token});
    _ ->
        CONFIG
end.
```

Example `rebar.config`:

```erlang

{plugins                , [coveralls]}. % use hex package
{cover_enabled          , true}.
{cover_export_enabled   , true}.
{coveralls_coverdata    , "_build/test/cover/ct.coverdata"}.
{coveralls_service_name , "circle-ci"}.
```

Note that you'll need to set `COVERALLS_REPO_TOKEN` in your CircleCI environment variables!

## Example usage: rebar3 and GitHub Actions

In order to use coveralls-erl + GitHub Actions in your project, you will need to add the following lines to your
`rebar.config.script`:

```erlang
case {os:getenv("GITHUB_ACTIONS"), os:getenv("GITHUB_TOKEN")} of
  {"true", Token} when is_list(Token) ->
    CONFIG1 = [{coveralls_repo_token, Token},
               {coveralls_service_job_id, os:getenv("GITHUB_RUN_ID")},
               {coveralls_commit_sha, os:getenv("GITHUB_SHA")},
               {coveralls_service_number, os:getenv("GITHUB_RUN_NUMBER")} | CONFIG],
    case os:getenv("GITHUB_EVENT_NAME") =:= "pull_request"
        andalso string:tokens(os:getenv("GITHUB_REF"), "/") of
        [_, "pull", PRNO, _] ->
            [{coveralls_service_pull_request, PRNO} | CONFIG1];
        _ ->
            CONFIG1
    end;
  _ ->
    CONFIG
end.
```

This will ensure that the rebar coveralls plugin will have access to the needed JobId and that the plugin is only run from GitHub Actions.

You will also need to add the following lines to your `rebar.config`:
```erlang
{plugins                , [coveralls]}. % use hex package
{cover_enabled          , true}.
{cover_export_enabled   , true}.
{coveralls_coverdata    , "_build/test/cover/eunit.coverdata"}. % or a string with wildcards or a list of files
{coveralls_service_name , "github"}.
```

These changes will add `coveralls-erl` as a dependency, tell `rebar3` where to find the plugin, make sure that the coverage data is produced and exported and configure `coveralls-erl` to use this data and the service `github`.

And you send the coverdata to coveralls by adding a step like:

```
- name: Coveralls
  env:
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
  run: rebar3 as test coveralls send
```

Other available GitHub Actions Environment Variables are available [here](https://help.github.com/en/actions/configuring-and-managing-workflows/using-environment-variables)

## Optional settings

The pluging also support the `coveralls_service_pull_request` and `coveralls_parallel` settings.
See the Coveralls documentation for the meaning of those.

## Author
Markus Ekholm (markus at botten dot org).

## License
3-clause BSD. For details see `COPYING`.
