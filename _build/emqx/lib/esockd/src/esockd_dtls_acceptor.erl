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

-module(esockd_dtls_acceptor).

-behaviour(gen_statem).

-include("esockd.hrl").

-export([start_link/5]).

-export([ waiting_for_sock/3
        , waiting_for_data/3
        , suspending/3
        ]).

%% gen_statem callbacks
-export([ init/1
        , callback_mode/0
        , handle_event/4
        , terminate/3
        , code_change/4
        ]).

-record(state, {
          sup       :: pid(),
          mfargs    :: mfa(),
          max_conns :: non_neg_integer(),
          limit_fun :: fun(),
          peername  :: {inet:ip_address(), inet:port_number()},
          lsock     :: inet:socket(),
          sock      :: ssl:sslsocket(),
          channel   :: pid()
         }).

start_link(Sup, Opts, MFA, LimitFun, LSock) ->
    gen_statem:start_link(?MODULE, [Sup, Opts, MFA, LimitFun, LSock], []).

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------

init([Sup, Opts, MFA, LimitFun, LSock]) ->
    process_flag(trap_exit, true),
    State = #state{sup = Sup, mfargs = MFA, limit_fun = LimitFun,
                   max_conns = max_conns(Opts), lsock = LSock},
    {ok, waiting_for_sock, State, {next_event, internal, accept}}.

max_conns(Opts) ->
    proplists:get_value(max_connections, Opts, 0).

callback_mode() -> state_functions.

waiting_for_sock(internal, accept, State) ->
    rate_limit(fun accept/1, State);

waiting_for_sock(EventType, EventContent, StateData) ->
    handle_event(EventType, EventContent, waiting_for_sock, StateData).

waiting_for_data(info, {ssl, Sock, Data}, State = #state{sock = Sock, channel = Ch}) ->
    Ch ! {datagram, self(), Data},
    {keep_state, State};

waiting_for_data(info, {ssl_closed, _Sock}, State) ->
    {stop, {shutdown, closed}, State};

waiting_for_data(info, {datagram, _To, Data}, State = #state{sock = Sock}) ->
    case ssl:send(Sock, Data) of
        ok -> {keep_state, State};
        {error, Reason} ->
            shutdown(Reason, State)
    end;

waiting_for_data(info, {'EXIT', Ch, Reason}, State = #state{channel = Ch}) ->
    {stop, Reason, State};

waiting_for_data(EventType, EventContent, StateData) ->
    handle_event(EventType, EventContent, waiting_for_data, StateData).

suspending(timeout, _Timeout, State) ->
    {next_state, waiting_for_sock, State, {next_event, internal, accept}}.

handle_event(EventType, EventContent, StateName, StateData) ->
    error_logger:error_msg("[~s] StateName: ~s, unexpected event(~s, ~p)",
                           [?MODULE, StateName, EventType, EventContent]),
    {keep_state, StateData}.

terminate(_Reason, _StateName, #state{sock = undefined}) ->
    ok;
terminate(_Reason, _StateName, #state{sock = Sock}) ->
    ssl:close(Sock).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

accept(State = #state{sup = Sup, lsock = LSock, mfargs = {M, F, Args}}) ->
    {ok, Sock} = ssl:transport_accept(LSock),
    esockd_dtls_acceptor_sup:start_acceptor(Sup, LSock),
    {ok, Peername} = ssl:peername(Sock),
    case ssl:handshake(Sock, ?SSL_HANDSHAKE_TIMEOUT) of
        {ok, SslSock} ->
            try erlang:apply(M, F, [{dtls, self(), SslSock}, Peername | Args]) of
                {ok, Pid} ->
                    true = link(Pid),
                    {next_state, waiting_for_data,
                     State#state{sock = SslSock, peername = Peername, channel = Pid}};
                {error, Reason} ->
                    {stop, Reason, State}
            catch
                _Error:Reason ->
                    shutdown(Reason, State)
            end;
        {error, Reason} ->
            shutdown(Reason, State#state{sock = Sock})
    end.

rate_limit(Fun, State = #state{limit_fun = RateLimit}) ->
    case RateLimit(1) of
        I when I =< 0 ->
            {next_state, suspending, State, 1000};
        _ -> Fun(State)
    end.

shutdown(Reason, State) ->
    {stop, {shutdown, Reason}, State}.

