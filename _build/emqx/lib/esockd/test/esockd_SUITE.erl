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

-module(esockd_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("esockd.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> esockd_ct:all(?MODULE).

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(esockd),
    Config.

end_per_suite(_Config) ->
    application:stop(esockd).

t_open_close(_) ->
    {ok, _LSup} = esockd:open(echo, {"127.0.0.1", 3000}, [binary, {packet, raw}],
                              {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect("127.0.0.1", 3000, [binary, {active, false}]),
    ok = gen_tcp:send(Sock, <<"Hello">>),
    {ok, <<"Hello">>} = gen_tcp:recv(Sock, 0),
    ok = esockd:close(echo, {"127.0.0.1", 3000}).

t_reopen(_) ->
    {ok, _LSup} = esockd:open(echo, {"127.0.0.1", 3000}, [binary, {packet, raw}],
                              {echo_server, start_link, []}),
    {ok, Sock1} = gen_tcp:connect("127.0.0.1", 3000, [{active, false}]),
    ok = gen_tcp:send(Sock1, <<"Hello">>),
    timer:sleep(10),
    ok = esockd:reopen({echo, {"127.0.0.1", 3000}}),
    {ok, Sock2} = gen_tcp:connect("127.0.0.1", 3000, [{active, false}]),
    ok = gen_tcp:send(Sock2, <<"Hello">>),
    timer:sleep(10),
    ok = esockd:close(echo, {"127.0.0.1", 3000}).

t_reopen_1(_) ->
    {ok, _LSup} = esockd:open(echo, 7000, [{max_connections, 4}, {acceptors, 4}],
                              {echo_server, start_link, []}),
    timer:sleep(10),
    ok = esockd:reopen({echo, 7000}),
    ?assertEqual(4, esockd:get_max_connections({echo, 7000})),
    ?assertEqual(4, esockd:get_acceptors({echo, 7000})),
    ok = esockd:close(echo, 7000).

t_reopen_fail(_) ->
    {ok, _LSup} = esockd:open(echo, 4000, [{acceptors, 4}], {echo_server, start_link, []}),
    {error, not_found} = esockd:reopen({echo, 5000}),
    ?assertEqual(4, esockd:get_acceptors({echo, 4000})),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 4000, [binary, {active, false}]),
    ok = gen_tcp:send(Sock, <<"Hello">>),
    {ok, <<"Hello">>} = gen_tcp:recv(Sock, 0),
    ok = esockd:close(echo, 4000).

t_open_udp(_) ->
    {ok, _} = esockd:open_udp(echo, 5678, [], {udp_echo_server, start_link, []}),
    {ok, Sock} = gen_udp:open(0, [binary, {active, false}]),
    ok = gen_udp:send(Sock, {127,0,0,1}, 5678, <<"Hi">>),
    {ok, {_Addr, 5678, <<"Hi">>}} = gen_udp:recv(Sock, 0),
    ok = esockd:close(echo, 5678).

t_udp_child_spec(_) ->
    Spec = esockd:udp_child_spec(echo, 5000, [], {udp_echo_server, start_link, []}),
    #{id := {listener_sup,{echo,5000}},
      modules := [esockd_udp],
      restart := transient,
      shutdown := 5000,
      type := worker
     } = Spec.

t_open_dtls(Config) ->
    DtlsOpts = [{mode, binary},
                {reuseaddr, true},
                {certfile, esockd_ct:certfile(Config)},
                {keyfile,  esockd_ct:keyfile(Config)}
               ],
    {ok, _} = esockd:open_dtls(echo, 5000, [{dtls_options, DtlsOpts}],
                               {udp_echo_server, start_link, []}),
    {ok, Sock} = ssl:connect({127,0,0,1}, 5000, [binary,
                                                 {protocol, dtls},
                                                 {active, false}
                                                ], 5000),
    ok = ssl:send(Sock, <<"Hi">>),
    {ok, <<"Hi">>} = ssl:recv(Sock, 0, 3000),
    ok = ssl:close(Sock),
    ok = esockd:close(echo, 5000).

t_dtls_child_spec(_) ->
    Spec = esockd:dtls_child_spec(echo, 8883, [], {udp_echo_server, start_link, []}),
     #{id := {listener_sup,{echo,8883}},
       modules := [esockd_dtls_listener_sup],
       restart := transient,
       shutdown := infinity,
       type := supervisor
      } = Spec.

t_child_spec(_) ->
    Spec = esockd:child_spec(echo, 5000, [], {echo_server, start_link, []}),
    #{id := {listener_sup, {echo,5000}},
      modules := [esockd_listener_sup],
      restart := transient,
      shutdown := infinity,
      type := supervisor
     } = Spec.

t_listeners(_) ->
    {ok, LSup} = esockd:open(echo, 6000, [], {echo_server, start_link, []}),
    [{{echo, 6000}, LSup}] = esockd:listeners(),
    ?assertEqual(LSup, esockd:listener({echo, 6000})),
    ok = esockd:close(echo, 6000),
    [] = esockd:listeners(),
    ?assertEqual(undefined, esockd:listener({echo, 6000})).

t_get_stats(_) ->
    {ok, _LSup} = esockd:open(echo, 6000, [], {echo_server, start_link, []}),
    {ok, Sock1} = gen_tcp:connect("127.0.0.1", 6000, [{active, false}]),
    {ok, Sock2} = gen_tcp:connect("127.0.0.1", 6000, [{active, false}]),
    timer:sleep(10),
    [{accepted, 2}] = esockd:get_stats({echo, 6000}),
    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    ok = esockd:close(echo, 6000).

t_get_options(_) ->
    {ok, _LSup} = esockd:open(echo, 6000, [{acceptors, 4}],
                              {echo_server, start_link, []}),
    [{acceptors, 4}] = esockd:get_options({echo, 6000}),
    ok = esockd:close(echo, 6000).

t_get_acceptors(_) ->
    {ok, _LSup} = esockd:open(echo, {{127,0,0,1}, 6000}, [{acceptors, 4}],
                              {echo_server, start_link, []}),
    ?assertEqual(4, esockd:get_acceptors({echo, {{127,0,0,1}, 6000}})),
    ok = esockd:close(echo, 6000).

t_get_set_max_connections(_) ->
    {ok, _LSup} = esockd:open(echo, 7000, [{max_connections, 4}],
                              {echo_server, start_link, []}),
    ?assertEqual(4, esockd:get_max_connections({echo, 7000})),
    esockd:set_max_connections({echo, 7000}, 16),
    ?assertEqual(16, esockd:get_max_connections({echo, 7000})),
    ok = esockd:close(echo, 7000).

t_get_current_connections(_) ->
    {ok, _LSup} = esockd:open(echo, 7000, [], {echo_server, start_link, []}),
    {ok, Sock1} = gen_tcp:connect("127.0.0.1", 7000, [{active, false}]),
    {ok, Sock2} = gen_tcp:connect("127.0.0.1", 7000, [{active, false}]),
    ?assertEqual(2, esockd:get_current_connections({echo, 7000})),
    ok = gen_tcp:close(Sock1),
    ok = gen_tcp:close(Sock2),
    timer:sleep(10),
    ?assertEqual(0, esockd:get_current_connections({echo, 7000})),
    ok = esockd:close(echo, 7000).

t_get_shutdown_count(_) ->
    {ok, _LSup} = esockd:open(echo, 7000, [], {echo_server, start_link, []}),
    {ok, Sock1} = gen_tcp:connect("127.0.0.1", 7000, [{active, false}]),
    {ok, Sock2} = gen_tcp:connect("127.0.0.1", 7000, [{active, false}]),
    ok = gen_tcp:close(Sock1),
    ok = gen_tcp:close(Sock2),
    timer:sleep(10),
    ?assertEqual([{closed, 2}], esockd:get_shutdown_count({echo, 7000})),
    ok = esockd:close(echo, 7000).

t_allow_deny(_) ->
    AccessRules = [{allow, "192.168.1.0/24"}],
    {ok, _LSup} = esockd:open(echo, 7000, [{access_rules, AccessRules}],
                              {echo_server, start_link, []}),
    ?assertEqual([{allow, "192.168.1.0/24"}], esockd:get_access_rules({echo, 7000})),
    ok = esockd:allow({echo, 7000}, "10.10.0.0/16"),
    ?assertEqual([{allow, "10.10.0.0/16"},
                  {allow, "192.168.1.0/24"}
                 ], esockd:get_access_rules({echo, 7000})),
    ok = esockd:deny({echo, 7000}, "172.16.1.1/16"),
    ?assertEqual([{deny,  "172.16.0.0/16"},
                  {allow, "10.10.0.0/16"},
                  {allow, "192.168.1.0/24"}
                 ], esockd:get_access_rules({echo, 7000})),
    ok = esockd:close(echo, 7000).

t_ulimit(_) ->
    ?assert(is_integer(esockd:ulimit())).

t_merge_opts(_) ->
    Opts = [binary, {acceptors, 8}, {tune_buffer, true}],
    ?assertEqual([binary, {acceptors, 16}, {tune_buffer, true}],
                 esockd:merge_opts(Opts, [{acceptors, 16}])).

t_parse_opt(_) ->
    Opts = [{acceptors, 10}, {tune_buffer, true}, {proxy_protocol, true}, {ssl_options, []}],
    ?assertEqual(Opts, esockd:parse_opt([{badopt1, val1}, {badopt2, val2}|Opts])).

t_fixaddr(_) ->
    ?assertEqual({{127,0,0,1}, 9000}, esockd:fixaddr({"127.0.0.1", 9000})),
    ?assertEqual({{10,10,10,10}, 9000}, esockd:fixaddr({{10,10,10,10}, 9000})),
    ?assertEqual({{0,0,0,0,0,0,0,1}, 9000}, esockd:fixaddr({"::1", 9000})).

t_to_string(_) ->
    ?assertEqual("9000", esockd:to_string(9000)),
    ?assertEqual("127.0.0.1:9000", esockd:to_string({"127.0.0.1", 9000})),
    ?assertEqual("192.168.1.10:9000", esockd:to_string({{192,168,1,10}, 9000})),
    ?assertEqual("::1:9000", esockd:to_string({{0,0,0,0,0,0,0,1}, 9000})).

t_format(_) ->
    ?assertEqual("127.0.0.1:9000", esockd:format({{127,0,0,1}, 9000})),
    ?assertEqual("::1:9000", esockd:format({{0,0,0,0,0,0,0,1}, 9000})).

