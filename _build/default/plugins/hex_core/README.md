# hex_core

[![Build Status](https://github.com/hexpm/hex_core/workflows/CI/badge.svg)](https://github.com/hexpm/hex_core/actions)

Reference implementation of Hex specifications: https://github.com/hexpm/specifications.

## Usage

Let's use default config for now. See "Configuration" section below for customization.

```erlang
Config = hex_core:default_config().
```

### Repository

Get all package names:

```erlang
> hex_repo:get_names(Config).
{ok, {200, ...,
    #{packages => [
        #{name => <<"package1">>},
        #{name => <<"package2">>},
        ...
    ]}}}
```

Get all package versions from repository:

```erlang
> hex_repo:get_versions(Config).
{ok, {200, ...,
    [
        #{name => <<"package1">>, retired => [], versions => [<<"1.0.0">>]},
        #{name => <<"package2">>, retired => [], versions => [<<"0.5.0">>]},
    ]}}
```

Get package releases from repository:

```erlang
> hex_repo:get_package(Config, <<"package1">>).
{ok, {200, ...,
    [
        #{checksum => ..., version => <<"0.5.0">>, dependencies => []}],
        #{checksum => ..., version => <<"1.0.0">>, dependencies => []}],
    ]}}
```

### API

For a full list of all parameters and returned objects for the API, check out the API docs:  https://github.com/hexpm/specifications/blob/master/http_api.md.

Get package from HTTP API:

```erlang
> hex_api_package:get(Config, <<"package1">>).
{ok, {200, ...,
    #{
        <<"name">> => <<"package1">>,
        <<"meta">> => #{
           <<"description">> => ...,
           <<"licenses">> => ...,
           <<"links">> => ...,
           <<"maintainers">> => ...,
        },
        ...,
        <<"releases">> => [
            #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
            #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
            ...
        ]
    }}}
```

Get package tarball:

```erlang
{ok, {200, _, Tarball}} = hex_repo:get_tarball(Config, <<"package1">>, <<"1.0.0">>).
```

Publish package tarball:

```erlang
{ok, {200, _Headers, _Body} = hex_api_package:publish(Config, Tarball).
```

### Package tarballs

Unpack package tarball:

```erlang
{ok, #{outer_checksum := Checksum, contents := Contents, metadata := Metadata}} = hex_tarball:unpack(Tarball, memory).
```

Remember to verify the outer tarball checksum against the registry checksum
returned from `hex_repo:get_package(Config, Package)`.

Create package tarball:

```erlang
{ok, #{tarball := Tarball,
       inner_checksum := InnerChecksum,
       outer_checksum := OuterChecksum}} = hex_tarball:create(Metadata, Contents).
```

## Configuration

The default configuration, provided by `hex_core:default_config/0`, uses built-in httpc-based adapter and Hex.pm APIs:
<https://hex.pm/api> and <https://repo.hex.pm>.

HTTP client configuration can be overridden as follows:

```erlang
Config = maps:merge(hex_core:default_config(), #{
  http_adapter => {my_hackney_adapter, my_hackney_adapter_config},
  http_user_agent_fragment => <<"(my_app/0.1.0) (hackney/1.12.1) ">>
}),
hex_repo:get_names(Config).

%% my_hackney_adapter.erl
-module(my_hackney_adapter).
-behaviour(hex_http).
-exports([request/3]).

request(Method, URI, ReqHeaders) ->
    %% ...
```

See the [`hex_core` module](src/hex_core.erl) for more information about the configuration.

## Wrapper Module

It's recommended to write a wrapper module because a lot of decisions are left to the user, e.g.:
where to get configuration from, how to handle caching, failures etc.

For a sample, see: [`examples/myapp_hex.erl`](examples/myapp_hex.erl). Here's an excerpt:

```erlang
-module(myapp_hex).
-export([
    get_api_package/1,
    get_repo_tarball/2,
    get_repo_versions/0
]).

%%====================================================================
%% API functions
%%====================================================================

get_api_package(Name) ->
      case hex_api_package:get(config(), Name) of
          {ok, {200, _Headers, Payload}} ->
              {ok, Payload};

          Other ->
              Other
      end.

get_repo_versions() ->
      case hex_repo:get_versions(config()) of
          {ok, {200, _Headers, Payload}} ->
              {ok, maps:get(packages, Payload)};

          Other ->
              Other
      end.

%%====================================================================
%% Internal functions
%%====================================================================

config() ->
    Config1 = hex_core:default_config(),
    Config2 = put_http_config(Config1),
    Config3 = maybe_put_api_key(Config2),
    Config3.

put_http_config(Config) ->
    maps:put(http_user_agent_fragment, <<"(myapp/1.0.0) (httpc)">>, Config).

maybe_put_api_key(Config) ->
    case os:getenv("HEX_API_KEY") of
        false -> Config;
        Key -> maps:put(api_key, Key, Config)
    end.
```

## Installation

### Rebar3

Add to `rebar.config`:

```erlang
{deps, [
  {hex_core, "0.7.1"}
]}
```

### Mix

Add to `mix.exs`:

```elixir
defp deps() do
  [
    {:hex_core, "~> 0.7.1"}
  ]
end
```

## Development

* Run `rebar3 as dev compile` to re-generate protobuf files
* Run `rebar3 as test proper` for property-based tests
* Run `rebar3 as docs edoc` to generate documentation
