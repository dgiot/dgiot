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

-module(esockd_acceptor).

-behaviour(gen_statem).

-include("esockd.hrl").

-export([start_link/6]).

%% state callbacks
-export([ accepting/3
        , suspending/3
        ]).

%% gen_statem Callbacks
-export([ init/1
        , callback_mode/0
        , terminate/3
        , code_change/4
        ]).

-record(state, {
          lsock        :: inet:socket(),
          sockmod      :: module(),
          sockname     :: {inet:ip_address(), inet:port_number()},
          tune_fun     :: esockd:sock_fun(),
          upgrade_funs :: [esockd:sock_fun()],
          stats_fun    :: fun(),
          limit_fun    :: fun(),
          conn_sup     :: pid(),
          accept_ref   :: term()
        }).

%% @doc Start an acceptor
-spec(start_link(pid(), esockd:sock_fun(), [esockd:sock_fun()], fun(), fun(), inet:socket())
      -> {ok, pid()} | {error, term()}).
start_link(ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun, LSock) ->
    gen_statem:start_link(?MODULE, [ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun, LSock], []).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([ConnSup, TuneFun, UpgradeFuns, StatsFun, LimitFun, LSock]) ->
    _ = rand:seed(exsplus, erlang:timestamp()),
    {ok, Sockname} = inet:sockname(LSock),
    {ok, SockMod} = inet_db:lookup_socket(LSock),
    {ok, accepting, #state{lsock        = LSock,
                           sockmod      = SockMod,
                           sockname     = Sockname,
                           tune_fun     = TuneFun,
                           upgrade_funs = UpgradeFuns,
                           stats_fun    = StatsFun,
                           limit_fun    = LimitFun,
                           conn_sup     = ConnSup},
     {next_event, internal, accept}}.

callback_mode() -> state_functions.

accepting(internal, accept, State = #state{lsock = LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} ->
            {keep_state, State#state{accept_ref = Ref}};
        {error, Reason} when Reason =:= emfile;
                             Reason =:= enfile ->
            {next_state, suspending, State, 1000};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            {stop, Reason, State}
    end;

accepting(info, {inet_async, LSock, Ref, {ok, Sock}},
          State = #state{lsock        = LSock,
                         sockmod      = SockMod,
                         sockname     = Sockname,
                         tune_fun     = TuneFun,
                         upgrade_funs = UpgradeFuns,
                         stats_fun    = StatsFun,
                         conn_sup     = ConnSup,
                         accept_ref   = Ref}) ->
    %% make it look like gen_tcp:accept
    inet_db:register_socket(Sock, SockMod),

    %% Inc accepted stats.
    StatsFun({inc, 1}),

    case TuneFun(Sock) of
        {ok, Sock} ->
            case esockd_connection_sup:start_connection(ConnSup, Sock, UpgradeFuns) of
                {ok, _Pid} -> ok;
                {error, enotconn} ->
                    close(Sock); %% quiet...issue #10
                {error, einval} ->
                    close(Sock); %% quiet... haproxy check
                {error, Reason} ->
                    error_logger:error_msg("Failed to start connection on ~s: ~p",
                                           [esockd:format(Sockname), Reason]),
                    close(Sock)
                end;
        {error, enotconn} ->
            close(Sock);
        {error, einval} ->
            close(Sock);
        {error, closed} ->
            close(Sock);
        {error, Reason} ->
            error_logger:error_msg("Tune buffer failed on ~s: ~s",
                                   [esockd:format(Sockname), Reason]),
            close(Sock)
    end,
    rate_limit(State);

accepting(info, {inet_async, LSock, Ref, {error, closed}},
          State = #state{lsock = LSock, accept_ref = Ref}) ->
    {stop, normal, State};

%% {error, econnaborted} -> accept
%% {error, esslaccept}   -> accept
accepting(info, {inet_async, LSock, Ref, {error, Reason}},
          #state{lsock = LSock, accept_ref = Ref})
    when Reason =:= econnaborted; Reason =:= esslaccept ->
    {keep_state_and_data, {next_event, internal, accept}};

%% emfile: The per-process limit of open file descriptors has been reached.
%% enfile: The system limit on the total number of open files has been reached.
accepting(info, {inet_async, LSock, Ref, {error, Reason}},
          State = #state{lsock = LSock, sockname = Sockname, accept_ref = Ref})
    when Reason =:= emfile; Reason =:= enfile ->
    error_logger:error_msg("Accept error on ~s: ~s",
                           [esockd:format(Sockname), Reason]),
    {next_state, suspending, State, 1000};

accepting(info, {inet_async, LSock, Ref, {error, Reason}},
          State = #state{lsock = LSock, accept_ref = Ref}) ->
    {stop, Reason, State}.

suspending(timeout, _Timeout, State) ->
    {next_state, accepting, State, {next_event, internal, accept}}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

close(Sock) -> catch port_close(Sock).

rate_limit(State = #state{limit_fun = RateLimit}) ->
    case RateLimit(1) of
        {I, Pause} when I =< 0 ->
            {next_state, suspending, State, Pause};
        _ ->
            {keep_state, State, {next_event, internal, accept}}
    end.

