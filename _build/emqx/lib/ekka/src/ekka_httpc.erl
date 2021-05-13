%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%%--------------------------------------------------------------------

-module(ekka_httpc).

-export([ get/3
        , get/4
        , get/5
        , post/3
        , post/4
        , put/3
        , put/4
        , delete/3
        , delete/4
        ]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

get(Addr, Path, Params) ->
    get(Addr, Path, Params, []).

get(Addr, Path, Params, Headers) ->
    get(Addr, Path, Params, Headers, []).

get(Addr, Path, Params, Headers, HttpOpts) ->
    Req = {build_url(Addr, Path, Params), Headers},
    parse_response(httpc:request(get, Req, [{autoredirect, true} | HttpOpts], [])).

post(Addr, Path, Params) ->
    post(Addr, Path, Params, []).

post(Addr, Path, Params, HttpOpts) ->
    Req = {build_url(Addr, Path), [], "application/x-www-form-urlencoded", build_query(Params)},
    parse_response(httpc:request(post, Req, [{autoredirect, true} | HttpOpts], [])).

put(Addr, Path, Params) ->
    put(Addr, Path, Params, []).

put(Addr, Path, Params, HttpOpts) ->
    Req = {build_url(Addr, Path), [], "application/x-www-form-urlencoded", build_query(Params)},
    parse_response(httpc:request(put, Req, [{autoredirect, true} | HttpOpts], [])).

delete(Addr, Path, Params) ->
    delete(Addr, Path, Params, []).

delete(Addr, Path, Params, HttpOpts) ->
    Req = {build_url(Addr, Path, Params), []},
    parse_response(httpc:request(delete, Req, HttpOpts, [])).

-spec(build_url(string(), string()) -> string()).
build_url(Addr, Path) ->
    lists:concat([Addr, "/", Path]).

build_url(Addr, Path, Params) ->
    lists:concat([build_url(Addr, Path), "?", build_query(Params)]).

build_query(Params) ->
    string:join([urlencode(Param) || Param <- Params], "&").

urlencode(L) when is_list(L) ->
    http_uri:encode(L);
urlencode({K, V}) ->
    urlencode(K) ++ "=" ++ urlencode(V);
urlencode(A) when is_atom(A) ->
    urlencode(atom_to_list(A));
urlencode(I) when is_integer(I) ->
    urlencode(integer_to_list(I));
urlencode(B) when is_binary(B) ->
    urlencode(binary_to_list(B)).

parse_response({ok, {{_, Code, _}, _Headers, Body}}) ->
    parse_response({ok, Code, Body});
parse_response({ok, {Code, Body}}) ->
    parse_response({ok, Code, Body});
parse_response({ok, 200, Body}) ->
    {ok, jsx:decode(iolist_to_binary(Body), [return_maps])};
parse_response({ok, 201, Body}) ->
    {ok, jsx:decode(iolist_to_binary(Body), [return_maps])};
parse_response({ok, 204, _Body}) ->
    {ok, []};
parse_response({ok, Code, Body}) ->
    {error, {Code, Body}};
parse_response({error, Reason}) ->
    {error, Reason}.

