%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(esockd_listener_sup).

-behaviour(supervisor).

-include("esockd.hrl").

-export([ start_link/5
        , listener/1
        , acceptor_sup/1
        , connection_sup/1
       ]).

%% get/set
-export([ get_options/1
        , get_acceptors/1
        , get_max_connections/1
        , get_current_connections/1
        , get_shutdown_count/1
        ]).

-export([ set_max_connections/2 ]).

-export([ get_access_rules/1
        , allow/2
        , deny/2
        ]).

%% supervisor callbacks
-export([init/1]).

-type listen_type() :: tcp | dtls.

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

%% @doc Start listener supervisor
-spec(start_link(listen_type(), atom(), esockd:listen_on(), [esockd:option()], esockd:mfargs())
      -> {ok, pid()} | {error, term()}).
start_link(Type, Proto, ListenOn, Opts, MFA) ->
    {ok, Sup} = supervisor:start_link(?MODULE, []),

    %% Start connection sup
    ConnSupSpec = #{id => connection_sup,
                    start => {esockd_connection_sup, start_link, [Opts, MFA]},
                    restart => transient,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [esockd_connection_sup]},
    {ok, ConnSup} = supervisor:start_child(Sup, ConnSupSpec),

    %% Start acceptor sup
    TuneFun = buffer_tune_fun(Opts),
    UpgradeFuns = upgrade_funs(Type, Opts),
    StatsFun = esockd_server:stats_fun({Proto, ListenOn}, accepted),
    LimitFun = rate_limit_fun({listener, Proto, ListenOn}, Opts),

    AcceptorSupMod = case Type of
                         dtls -> esockd_dtls_acceptor_sup;
                         _ -> esockd_acceptor_sup
                     end,
    AcceptorSupSpec = #{id => acceptor_sup,
                        start => {AcceptorSupMod, start_link,
                                  [ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun]},
                        restart => transient,
                        shutdown => infinity,
                        type => supervisor,
                        modules => [AcceptorSupMod]},
    {ok, AcceptorSup} = supervisor:start_child(Sup, AcceptorSupSpec),

    %% Start listener
    ListenerMod = case Type of
                      dtls -> esockd_dtls_listener;
                      _ -> esockd_listener
                  end,
    ListenerSpec = #{id => listener,
                     start => {ListenerMod, start_link,
                               [Proto, ListenOn, Opts, AcceptorSup]},
                     restart => transient,
                     shutdown => 16#ffffffff,
                     type => worker,
                     modules => [ListenerMod]},
    case supervisor:start_child(Sup, ListenerSpec) of
        {ok, _} -> {ok, Sup};
        {error, {Reason, _ChildSpec}} ->
            {error, Reason}
    end.

%% @doc Get listener.
-spec(listener(pid()) -> pid()).
listener(Sup) -> child_pid(Sup, listener).

%% @doc Get connection supervisor.
-spec(connection_sup(pid()) -> pid()).
connection_sup(Sup) -> child_pid(Sup, connection_sup).

%% @doc Get acceptor supervisor.
-spec(acceptor_sup(pid()) -> pid()).
acceptor_sup(Sup) -> child_pid(Sup, acceptor_sup).

%% @doc Get child pid with id.
child_pid(Sup, ChildId) ->
    hd([Pid || {Id, Pid, _, _}
               <- supervisor:which_children(Sup), Id =:= ChildId]).

%%--------------------------------------------------------------------
%% Get/Set APIs
%%--------------------------------------------------------------------

get_options(Sup) ->
    esockd_listener:options(listener(Sup)).

get_acceptors(Sup) ->
    esockd_acceptor_sup:count_acceptors(acceptor_sup(Sup)).

get_max_connections(Sup) ->
    esockd_connection_sup:get_max_connections(connection_sup(Sup)).

set_max_connections(Sup, MaxConns) ->
    esockd_connection_sup:set_max_connections(connection_sup(Sup), MaxConns).

get_current_connections(Sup) ->
    esockd_connection_sup:count_connections(connection_sup(Sup)).

get_shutdown_count(Sup) ->
    esockd_connection_sup:get_shutdown_count(connection_sup(Sup)).

get_access_rules(Sup) ->
    esockd_connection_sup:access_rules(connection_sup(Sup)).

allow(Sup, CIDR) ->
    esockd_connection_sup:allow(connection_sup(Sup), CIDR).

deny(Sup, CIDR) ->
    esockd_connection_sup:deny(connection_sup(Sup), CIDR).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, {{rest_for_one, 10, 3600}, []}}.

%%--------------------------------------------------------------------
%% Sock tune/upgrade functions
%%--------------------------------------------------------------------

buffer_tune_fun(Opts) ->
    buffer_tune_fun(proplists:get_value(buffer, Opts),
                    proplists:get_bool(tune_buffer, Opts)).

%% when 'buffer' is undefined, and 'tune_buffer' is enabled...
buffer_tune_fun(undefined, true) ->
    fun(Sock) ->
        case esockd_transport:getopts(Sock, [sndbuf, recbuf, buffer]) of
            {ok, BufSizes} ->
                BufSz = lists:max([Sz || {_Opt, Sz} <- BufSizes]),
                _ = esockd_transport:setopts(Sock, [{buffer, BufSz}]),
                {ok, Sock};
            Error -> Error
        end
    end;
buffer_tune_fun(_, _) ->
    fun(Sock) -> {ok, Sock} end.

upgrade_funs(Type, Opts) ->
    lists:append([ssl_upgrade_fun(Type, Opts), proxy_upgrade_fun(Opts)]).

ssl_upgrade_fun(Type, Opts) ->
    Key = case Type of
              dtls -> dtls_options;
              _ -> ssl_options
          end,
    case proplists:get_value(Key, Opts) of
        undefined -> [];
        SslOpts -> [esockd_transport:ssl_upgrade_fun(SslOpts)]
    end.

proxy_upgrade_fun(Opts) ->
    case proplists:get_bool(proxy_protocol, Opts) of
        false -> [];
        true  -> [esockd_transport:proxy_upgrade_fun(Opts)]
    end.

rate_limit_fun(Bucket, Opts) ->
    case proplists:get_value(max_conn_rate, Opts) of
        undefined ->
            fun(_) -> {1, 0} end;
        I when is_integer(I) ->
            rate_limit_fun(Bucket, I, 1);
        {Capacity, Interval} ->
            rate_limit_fun(Bucket, Capacity, Interval)
    end.

rate_limit_fun(Bucket, Capacity, Interval) ->
    ok = esockd_limiter:create(Bucket, Capacity, Interval),
    fun(I) -> esockd_limiter:consume(Bucket, I) end.

