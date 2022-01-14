%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_transport).
-author("johnliu").
%% API
-export([get_opts/2, get_opts/3, get_port/1]).

-define(SOCKOPTS, [
    {reuseaddr, true},
    {backlog, 1024},
    {nodelay, false},
    {keepalive, false},
    {send_timeout, 5000},
    {send_timeout_close, true}
]).

-define(OPTS, [
    {tcp_options, ?SOCKOPTS},
    {acceptors, 8},
    {max_connections, 1024000},
    {max_conn_rate, 500}
]).


%% 获取连接配置
get_opts(tcp, _Port) ->
    TCPOpts = [
        {backlog, 512},
        {keepalive, true},
        {send_timeout, 15000},
        {send_timeout_close, true},
        {nodelay, true},
        {reuseaddr, true},
        binary,
        {packet, raw},
        {exit_on_close, true}
    ],
    Opts = [
        {tcp_options, TCPOpts},
        {acceptors, 16},
        {max_connections, 1000000},
        {max_conn_rate, {1000, 1}}
    ],
    {ok, 10, {1024, 4096}, Opts};

%% 获取连接配置
get_opts(udp, _Port) ->
    UDPOpts = [
        {reuseaddr, true}
    ],
    Opts = [
        {udp_options, UDPOpts},
        {access_rules, [{allow, all}]},
        {acceptors, 16},
        {max_connections, 1000000},
        {max_conn_rate, {1000, 1}}
    ],
    {ok, 10, {1024, 4096}, Opts}.

get_opts(tcp, App, _Port) ->
    Opts = application:get_env(App, listeners, ?OPTS),
    TCPOpts = proplists:get_value(tcp_options, Opts, ?SOCKOPTS) ++ [binary, {packet, raw}, {exit_on_close, true}],
    MaxConnections = proplists:get_value(max_connections, Opts, 1024000),
    MaxConnRate = proplists:get_value(max_conn_rate, Opts, 1024000),
    Acceptors = proplists:get_value(acceptors, Opts, 16),
    NewOpts = esockd:parse_opt([
        {tcp_options, TCPOpts},
        {acceptors, Acceptors},
        {max_connections, MaxConnections},
        {max_conn_rate, MaxConnRate}
    ]),
    ActiveN = proplists:get_value(active_n, Opts, 100),
    RateLimit = proplists:get_value(rate_limit, Opts, {1024, 4096}),
    {ok, ActiveN, RateLimit, NewOpts};

get_opts(udp, App, _Port) ->
    Opts = application:get_env(App, listeners, ?OPTS),
    TCPOpts = proplists:get_value(tcp_options, Opts, ?SOCKOPTS) ++ [binary, {packet, raw}, {exit_on_close, true}],
    MaxConnections = proplists:get_value(max_connections, Opts, 1024000),
    MaxConnRate = proplists:get_value(max_conn_rate, Opts, 1024000),
    Acceptors = proplists:get_value(acceptors, Opts, 16),
    NewOpts = esockd:parse_opt([
        {tcp_options, TCPOpts},
        {acceptors, Acceptors},
        {max_connections, MaxConnections},
        {max_conn_rate, MaxConnRate}
    ]),
    ActiveN = proplists:get_value(active_n, Opts, 100),
    RateLimit = proplists:get_value(rate_limit, Opts, {1024, 4096}),
    {ok, ActiveN, RateLimit, NewOpts}.

get_port(Port) ->
    Line = os:cmd(lists:concat(["netstat -nat|grep -i '", Port, "'|wc -l"])),
    case re:run(Line, "([0-9]+)\n", [{capture, all_but_first, list}]) of
        {match, [Num]} -> list_to_integer(Num);
        _ -> 0
    end.
