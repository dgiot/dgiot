-module(hex_api_package).
-export([get/2, search/3]).

%% @doc
%% Gets a package.
%%
%% Examples:
%%
%% ```
%% > hex_api_package:get(hex_core:default_config(), <<"package">>).
%% {ok, {200, ..., #{
%%     <<"name">> => <<"package1">>,
%%     <<"meta">> => #{
%%         <<"description">> => ...,
%%         <<"licenses">> => ...,
%%         <<"links">> => ...,
%%         <<"maintainers">> => ...
%%     },
%%     ...,
%%     <<"releases">> => [
%%         #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
%%         #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
%%         ...
%%     ]}}}
%% '''
%% @end
-spec get(hex_core:config(), binary()) -> hex_api:response().
get(Config, Name) when is_map(Config) and is_binary(Name)->
    Path = hex_api:build_repository_path(Config, ["packages", Name]),
    hex_api:get(Config, Path).

%% @doc
%% Searches packages.
%%
%% Examples:
%%
%% ```
%% > hex_api_package:search(hex_core:default_config(), <<"package">>, []).
%% {ok, {200, ..., [
%%     #{<<"name">> => <<"package1">>, ...},
%%     ...
%% ]}}
%% '''
-spec search(hex_core:config(), binary(), list(binary())) -> hex_api:response().
search(Config, Query, SearchParams) when is_map(Config) and is_binary(Query) and is_list(SearchParams) ->
    QueryString = hex_api:encode_query_string([{search, Query} | SearchParams]),
    Path = hex_api:join_path_segments(hex_api:build_repository_path(Config, ["packages"])),
    PathQuery = <<Path/binary, "?", QueryString/binary>>,
    hex_api:get(Config, PathQuery).
