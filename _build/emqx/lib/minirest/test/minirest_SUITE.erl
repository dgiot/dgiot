%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(minirest_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, handler}, {group, rest}, {group, rest_app}, t_return].

groups() ->
    [{handler,  [sequence], [t_init]},
     {rest,     [sequence], [t_index, t_get, t_list]},
     {rest_app, [sequence], [t_put, t_delete, t_auth]}].

init_per_suite(_Config) ->
    [application:ensure_all_started(App) || App <- [inets, cowboy, minirest]].

end_per_suite(_Config) ->
    ok.

init_per_group(rest, _Config) ->
    Handlers = [{"/api/v2/", minirest:handler(#{modules => [rest_api_books]})}],
    Dispatch = [{"/api/v2/[...]", minirest, Handlers}],
    minirest:start_http(rest_server, #{socket_opts => [{port, 8080}]} , Dispatch);

init_per_group(rest_app, _Config) ->
    ok = application:start(minirest_example),
    Handlers = [{"/api/v2/", minirest:handler(#{apps    => [minirest_example],
                                                modules => [rest_api_books]}),
                [{authorization, fun authorize_appid/1}]}],
    Dispatch = [{"/api/v2/[...]", minirest, Handlers}],
    minirest:start_http(rest_server, #{socket_opts => [{port, 8080}]} , Dispatch);

init_per_group(_Group, _Config) ->
    ok.

end_per_group(rest, _Config) ->
    minirest:stop_http(rest_server);

end_per_group(rest_app, _Config) ->
    ok = application:stop(minirest_example),
    minirest:stop_http(rest_server);

end_per_group(_Group, _Config) ->
    ok.

t_init(_Config) ->
    {minirest_handler, dispatch, [Routes, _Filter = undefined]} = minirest:handler(#{modules => [rest_api_books]}),
    ?assertEqual(4, length(Routes)).

t_index(_Config) ->
    {ok, {{_, 200, "OK"}, _Headers, Body}} = httpc_get("http://127.0.0.1:8080/api/v2/"),
    #{<<"code">> := 0, <<"data">> := APIs} = jsx:decode(Body, [return_maps]),
    ct:print("REST APIs: ~p", [APIs]),
    ?assertEqual(4, length(APIs)).

t_get(_Config) ->
    {ok, {{_, 200, "OK"}, _Headers, Body}} = httpc_get("http://127.0.0.1:8080/api/v2/books/1"),
    ?assertEqual(#{<<"id">> => 1, <<"name">> => <<"book1">>}, jsx:decode(Body, [return_maps])).

t_list(_Config) ->
    {ok, {{_, 200, "OK"}, _Headers, Body}} = httpc_get("http://127.0.0.1:8080/api/v2/books/"),
    ?assertEqual(100, length(jsx:decode(Body, [return_maps]))).

t_put(_Config) ->
    SuccessJson = [{<<"name">>, <<"ok">>}],
    ErrorJson = [{<<"name">>, <<"error">>}],
    {ok, {{_, 200, "OK"}, _Headers, Body}} = json_request(put, "http://127.0.0.1:8080/api/v2/books/1", SuccessJson, [auth_header_("admin", "public")]),
    ?assertEqual("\"ok\"", Body),
    {ok, {{_, ErrorCode, _}, _Headers1, _Body}} = json_request(put, "http://127.0.0.1:8080/api/v2/books/1", ErrorJson, [auth_header_("admin", "public")]),
    ?assertEqual(500, ErrorCode).

t_delete(_Config) ->
    {ok, {{_, 500, _}, _Headers, _Body}} = json_request(delete, "http://127.0.0.1:8080/api/v2/books/1", [], [auth_header_("admin", "public")]).

t_auth(_Config) ->
    {ok, {{_, 401, _}, _Headers, _Body}} = json_request(delete, "http://127.0.0.1:8080/api/v2/books/1",[], [auth_header_("admin1", "public")]).

t_return(_Config) ->
    ?assertEqual({ok, [{code, 0}]}, minirest:return()),
    ?assertEqual({ok, [{code, 0},
                       {data, <<"data">>},
                       {meta, #{}}]}, minirest:return({ok, #{data => <<"data">>, meta => #{}}})),

    ?assertEqual({ok, [{code, 0},
                       {data, <<"data">>}]}, minirest:return({ok, <<"data">>})),

    ?assertEqual({ok, [{code, 1001},
                       {message, <<"message">>}]}, minirest:return({ok, 1001, <<"message">>})),

    ?assertEqual({ok, [{code, 0},
                       {data, <<"data">>},
                       {meta, <<"meta">>}]}, minirest:return({ok, <<"data">>, <<"meta">>})),

    ?assertEqual({ok, [{message, <<"message">>}]}, minirest:return({error, <<"message">>})),
    ?assertEqual({ok, [{code, 1001},
                       {message, <<"message">>}]}, minirest:return({error, 1001, <<"message">>})).


httpc_get(Url) ->
    httpc:request(get, {Url, []}, [], [{body_format, binary}]).

json_request(Request, Url, Json) ->
    json_request(Request, Url, Json, []).

json_request(Request, Url, Json, Headers) ->
    ContentType = "application/json",
    Body = iolist_to_binary(jsx:encode(Json)),
    httpc:request(Request, {Url, Headers, ContentType, Body}, [], []).

auth_header_(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

authorize_appid(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, Username, Password} ->
            (Username =:= <<"admin">>) and (Password =:= <<"public">>);
         _  -> false
    end.

