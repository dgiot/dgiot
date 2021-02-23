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

-module(ekka_httpc_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

init_per_testcase(_TestCase, Config) ->
    ok = meck:new(httpc, [non_strict, passthrough, no_history]),
    Config.

end_per_testcase(_TestCase, Config) ->
    ok = meck:unload(httpc),
    Config.

t_get(_) ->
    ok = meck:expect(httpc, request, fun(get, _Req, _Opts, _) ->
                                             {ok, {{"HTTP/1.1", 204, "OK"}, [], <<"[]">>}}
                                     end),
    {ok, []} = ekka_httpc:get("localhost", "nodes", [{name, <<"node1">>}]),
    ok = meck:expect(httpc, request, fun(get, _Req, _Opts, _) ->
                                             {ok, {{"HTTP/1.1", 200, "OK"}, [], <<"{\"key\": \"value\"}">>}}
                                     end),
    {ok, #{<<"key">> := <<"value">>}} = ekka_httpc:get("localhost", "nodes", [{name, <<"node1">>}]).

t_post(_) ->
    ok = meck:expect(httpc, request, fun(post, _Req, _Opts, _) ->
                                             {ok, {201, <<"{\"code\": 0}">>}}
                                     end),
    {ok, #{<<"code">> := 0}} = ekka_httpc:post("localhost", "path", [{name, <<"x">>}]).

t_put(_) ->
    ok = meck:expect(httpc, request, fun(put, _Req, _Opts, _) ->
                                             {ok, {200, <<"{\"code\": 0}">>}}
                                     end),
    {ok, #{<<"code">> := 0}} = ekka_httpc:put("localhost", "path", [{name, <<"x">>}]).

t_delete(_) ->
    ok = meck:expect(httpc, request, fun(delete, _Req, _Opts, _) ->
                                             {ok, {200, <<"{\"code\": 0}">>}}
                                     end),
    {ok, #{<<"code">> := 0}} = ekka_httpc:delete("localhost", "path", [{name, <<"x">>}]).

t_build_url(_) ->
   ?assertEqual("localhost/nodes/1",
                ekka_httpc:build_url("localhost", "nodes/1")),
   ?assertEqual("localhost/nodes/1?a=b&c=d",
                ekka_httpc:build_url("localhost", "nodes/1", [{a, b}, {c, d}])),
   ?assertEqual("localhost/nodes/1?a=b%2Fc%2Fd",
                ekka_httpc:build_url("localhost", "nodes/1", [{a, "b/c/d"}])),
   ?assertEqual("localhost/nodes/1?a=b%2Fc%2Fd&e=5",
                ekka_httpc:build_url("localhost", "nodes/1", [{a, "b/c/d"}, {e, 5}])).

