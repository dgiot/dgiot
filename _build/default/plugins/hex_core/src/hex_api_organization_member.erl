-module(hex_api_organization_member).
-export([
    add/3,
    delete/2,
    get/2,
    list/1,
    update/3
]).

-type role() :: admin | write | read.

%% @doc
%% Lists the organization's members.
%%
%% Examples:
%%
%% ```
%% > hex_api_organization_member:list(hex_core:default_config()#{api_organization => <<"acme">>}).
%% {ok, {200, ..., [#{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"admin">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }]}}
%% '''
%% @end
-spec list(hex_core:config()) -> hex_api:response().
list(Config) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["members"]),
    hex_api:get(Config, Path).

%% @doc
%% Gets an organization member.
%%
%% Examples:
%%
%% ```
%% > hex_api_organization_member:get(hex_core:default_config()#{api_organization => <<"acme">>}, <<"user">>).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"admin">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec get(hex_core:config(), binary()) -> hex_api:response().
get(Config, UsernameOrEmail) when is_map(Config) and is_binary(UsernameOrEmail) ->
    Path = hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    hex_api:get(Config, Path).

%% @doc
%% Adds an organization member.
%%
%% Examples:
%%
%% ```
%% > hex_api_organization_member:add(hex_core:default_config()#{api_organization => <<"acme">>}, <<"user">>, write).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"write">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec add(hex_core:config(), binary(), role()) -> hex_api:response().
add(Config, UsernameOrEmail, Role) when is_map(Config) and is_binary(UsernameOrEmail) and is_atom(Role) ->
    Path = hex_api:build_organization_path(Config, ["members"]),
    Params = #{<<"name">> => UsernameOrEmail, <<"role">> => Role},
    hex_api:post(Config, Path, Params).

%% @doc
%% Updates an organization member's role.
%%
%% Examples:
%%
%% ```
%% > hex_api_organization_member:update(hex_core:default_config()#{api_organization => <<"acme">>}, <<"user">>, read).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"read">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec update(hex_core:config(), binary(), role()) -> hex_api:response().
update(Config, UsernameOrEmail, Role) when is_map(Config) and is_binary(UsernameOrEmail) and is_atom(Role) ->
    Path = hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    Params = #{<<"role">> => Role},
    hex_api:post(Config, Path, Params).

%% @doc
%% Deletes an organization member.
%%
%% Examples:
%%
%% ```
%% > hex_api_organization_member:delete(hex_core:default_config()#{api_organization => <<"acme">>}, <<"user">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec delete(hex_core:config(), binary()) -> hex_api:response().
delete(Config, UsernameOrEmail) when is_map(Config) and is_binary(UsernameOrEmail) ->
    Path = hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    hex_api:delete(Config, Path).
