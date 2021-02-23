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

-module(esockd_proxy_protocol_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("esockd.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> esockd_ct:all(?MODULE).

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(esockd),
    Config.

end_per_suite(_Config) ->
    application:stop(esockd).

init_per_testcase(TestCase, Config) ->
    case is_t_recv(TestCase) of
        true ->
            ok = meck:new(esockd_transport, [non_strict, passthrough, no_history]),
            ok = meck:expect(esockd_transport, setopts, fun(_Sock, _Opts) -> ok end),
            ok = meck:expect(esockd_transport, getopts, fun(_Sock, Opts) -> {ok, Opts} end);
        false -> ok
    end,
    Config.

end_per_testcase(TestCase, Config) ->
    is_t_recv(TestCase) andalso meck:unload(esockd_transport),
    Config.

is_t_recv(TestCase) ->
    string:substr(atom_to_list(TestCase), 1, 7) == "t_recv_".

%%--------------------------------------------------------------------
%% Test cases for recv
%%--------------------------------------------------------------------

t_recv_ppv1(_) ->
    ok = send_proxy_info(<<"PROXY TCP4 192.168.1.1 192.168.1.2 80 81\r\n">>),
    {ok, #proxy_socket{src_addr = {192,168,1,1}, src_port = 80,
                       dst_addr = {192,168,1,2}, dst_port = 81,
                       inet = inet4}} = recv_proxy_info().

t_recv_ppv1_unknown(_) ->
    ok = send_proxy_info(<<"PROXY UNKNOWN\r\n">>),
    {ok, sock} = recv_proxy_info().

t_recv_ppv2(_) ->
    ok = send_proxy_info(<<13,10>>),
    ok = meck:expect(esockd_transport, recv,
                     fun(sock, 14, _) -> {ok, <<13,10,0,13,10,81,85,73,84,10,33,17,0,12>>};
                        (sock, 12, _) -> {ok, <<127,50,210,1,210,21,16,142,250,32,1,181>>}
                     end),
    {ok, #proxy_socket{inet = inet4,
                       src_addr = {127,50,210,1},
                       dst_addr = {210,21,16,142},
                       src_port = 64032,
                       dst_port = 437}} = recv_proxy_info().

t_recv_pp_invalid(_) ->
    send_proxy_info(ProxyInfo = <<"Invalid PROXY\r\n">>),
    {error, {invalid_proxy_info, ProxyInfo}} = recv_proxy_info().

send_proxy_info(ProxyInfo) ->
    self() ! {esockd_transport, sock, ProxyInfo}, ok.

recv_proxy_info() ->
    esockd_proxy_protocol:recv(esockd_transport, sock, 1000).

%%--------------------------------------------------------------------
%% Test cases for parse
%%--------------------------------------------------------------------

t_parse_v1(_Config) ->
    {ok, #proxy_socket{src_addr = {192,168,1,30}, src_port = 45420,
                       dst_addr = {192,168,1,2},  dst_port = 1883}}
    = esockd_proxy_protocol:parse_v1(<<"192.168.1.30 192.168.1.2 45420 1883\r\n">>,
                                     #proxy_socket{}),
    {ok, #proxy_socket{src_addr = {255,255,255,255}, src_port = 65535,
                       dst_addr = {255,255,255,255}, dst_port = 65535}}
    = esockd_proxy_protocol:parse_v1(<<"255.255.255.255 255.255.255.255 65535 65535\r\n">>,
                                     #proxy_socket{}).

t_parse_v2(_Config) ->
    {ok, #proxy_socket{src_addr = {104,199,189,98}, src_port = 6000,
                       dst_addr = {106,185,34,253}, dst_port = 8883,
                       inet = inet4}}
    = esockd_proxy_protocol:parse_v2(16#1, 16#1, <<104,199,189,98,106,185,34,253,6000:16,8883:16>>,
                                     #proxy_socket{inet = inet4}),
    {ok, #proxy_socket{src_addr = {0,0,0,0,0,0,0,1}, src_port = 6000,
                       dst_addr = {0,0,0,0,0,0,0,1}, dst_port = 5000,
                       inet = inet6}}
    = esockd_proxy_protocol:parse_v2(16#1, 16#1, <<1:128, 1:128, 6000:16, 5000:16>>,
                                     #proxy_socket{inet = inet6}).

t_parse_pp2_additional(_) ->
    AdditionalInfo = [{pp2_alpn, <<"29zka">>},
                      {pp2_authority, <<"219a3k">>},
                      {pp2_netns, <<"abc.com">>},
                      {pp2_ssl,
                       [{pp2_ssl_client, true},
                        {pp2_ssl_client_cert_conn, true},
                        {pp2_ssl_client_cert_sess, true},
                        {pp2_ssl_verify, success}]}],
    {ok, #proxy_socket{src_addr = {104,199,189,98}, src_port = 6000,
                       dst_addr = {106,185,34,253}, dst_port = 8883,
                       inet = inet4, pp2_additional_info = AdditionalInfo}}
    = esockd_proxy_protocol:parse_v2(16#1, 16#1, <<104,199,189,98,106,185,34,253,6000:16,8883:16,
                                                   01,00,05,"29zka",02,00,06,"219a3k",16#30,00,07,"abc.com",
                                                   16#20,00,05,07,00,00,00,00>>,
                                     #proxy_socket{inet = inet4}).

t_parse_pp2_ssl(_) ->
    Bin = <<01,00,05,"29zka",02,00,06,"219a3k",16#30,00,07,"abc.com",16#20,00,05,03,00,00,00,01>>,
    [{pp2_ssl_client, false},
     {pp2_ssl_client_cert_conn, false},
     {pp2_ssl_client_cert_sess, true},
     {pp2_ssl_verify, success}|_]
    = esockd_proxy_protocol:parse_pp2_ssl(<<0:5, 1:1, 0:1, 0:1, 0:32, Bin/binary>>).

t_parse_pp2_tlv(_) ->
    Bin = <<01,00,05,"29zka",02,00,06,"219a3k",16#30,00,07,"abc.com",16#20,00,05,03,00,00,00,01>>,
    [{1, <<"29zka">>}, {2, <<"219a3k">>}, {16#30, <<"abc.com">>}, {16#20, <<3,0,0,0,1>>}]
    = esockd_proxy_protocol:parse_pp2_tlv(fun(E) -> E end, Bin).

%%--------------------------------------------------------------------
%% Test cases for pp server
%%--------------------------------------------------------------------

t_ppv1_tcp4_connect(_) ->
    with_pp_server(fun(Sock) ->
                           ok = gen_tcp:send(Sock, <<"PROXY TCP4 192.168.1.1 192.168.1.2 80 81\r\n">>),
                           ok = gen_tcp:send(Sock, <<"Hello">>),
                           {ok, <<"Hello">>} = gen_tcp:recv(Sock, 0)
                   end).

t_ppv1_tcp6_connect(_) ->
    with_pp_server(fun(Sock) ->
                           ok = gen_tcp:send(Sock, <<"PROXY TCP6 ::1 ::1 6000 50000\r\n">>),
                           ok = gen_tcp:send(Sock, <<"Hello">>),
                           {ok, <<"Hello">>} = gen_tcp:recv(Sock, 0)
                   end).

t_ppv2_connect(_) ->
    with_pp_server(fun(Sock) ->
                           ok = gen_tcp:send(Sock, <<13,10,13,10,0,13,10,81,85,73,84,10,33,17,0,
                                                     12,127,50,210,1,210,21,16,142,250,32,1,181>>),
                           ok = gen_tcp:send(Sock, <<"Hello">>),
                           {ok, <<"Hello">>} = gen_tcp:recv(Sock, 0)
                   end).

t_ppv1_unknown(_) ->
    with_pp_server(fun(Sock) ->
                           ok = gen_tcp:send(Sock, <<"PROXY UNKNOWN\r\n">>),
                           ok = gen_tcp:send(Sock, <<"Hello">>),
                           {ok, <<"Hello">>} = gen_tcp:recv(Sock, 0)
                   end).

t_ppv1_garbage_data(_) ->
    with_pp_server(fun(Sock) ->
                           ok = gen_tcp:send(Sock, <<"************\r\n">>),
                           ok = gen_tcp:send(Sock, <<"GarbageData">>)
                   end).

with_pp_server(TestFun) ->
    {ok, _} = esockd:open(echo, 5000, [{tcp_options, [binary]},
                                       proxy_protocol,
                                       {proxy_protocol_timeout, 3000}
                                      ],
                          {echo_server, start_link, []}),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, 5000, [binary, {active, false}]),
    try TestFun(Sock) after ok = esockd:close(echo, 5000) end.

