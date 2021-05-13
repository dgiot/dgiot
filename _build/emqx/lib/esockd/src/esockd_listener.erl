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

-module(esockd_listener).

-behaviour(gen_server).

-include("esockd.hrl").

-export([start_link/4]).

-export([ options/1
        , get_port/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {
          proto     :: atom(),
          listen_on :: esockd:listen_on(),
          options   :: [esockd:option()],
          lsock     :: inet:socket(),
          laddr     :: inet:ip_address(),
          lport     :: inet:port_number()
         }).

-define(ACCEPTOR_POOL, 16).
-define(DEFAULT_TCP_OPTIONS,
        [{nodelay, true},
         {reuseaddr, true},
         {send_timeout, 30000},
         {send_timeout_close, true}
        ]).

-spec(start_link(atom(), esockd:listen_on(), [esockd:option()], pid())
      -> {ok, pid()} | ignore | {error, term()}).
start_link(Proto, ListenOn, Opts, AcceptorSup) ->
    gen_server:start_link(?MODULE, {Proto, ListenOn, Opts, AcceptorSup}, []).

-spec(options(pid()) -> [esockd:option()]).
options(Listener) ->
    gen_server:call(Listener, options).

-spec(get_port(pid()) -> inet:port_number()).
get_port(Listener) ->
    gen_server:call(Listener, get_port).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init({Proto, ListenOn, Opts, AcceptorSup}) ->
    Port = port(ListenOn),
    process_flag(trap_exit, true),
    SockOpts = merge_addr(ListenOn, sockopts(Opts)),
    %% Don't active the socket...
    case esockd_transport:listen(Port, [{active, false} | proplists:delete(active, SockOpts)]) of
        {ok, LSock} ->
            AcceptorNum = proplists:get_value(acceptors, Opts, ?ACCEPTOR_POOL),
            lists:foreach(fun (_) ->
                {ok, _APid} = esockd_acceptor_sup:start_acceptor(AcceptorSup, LSock)
            end, lists:seq(1, AcceptorNum)),
            {ok, {LAddr, LPort}} = inet:sockname(LSock),
            %%error_logger:info_msg("~s listen on ~s:~p with ~p acceptors.~n",
            %%                      [Proto, inet:ntoa(LAddr), LPort, AcceptorNum]),
            {ok, #state{proto = Proto, listen_on = ListenOn, options = Opts,
                        lsock = LSock, laddr = LAddr, lport = LPort}};
        {error, Reason} ->
            error_logger:error_msg("~s failed to listen on ~p - ~p (~s)",
                                   [Proto, Port, Reason, inet:format_error(Reason)]),
            {stop, Reason}
    end.

sockopts(Opts) ->
    esockd:merge_opts(?DEFAULT_TCP_OPTIONS,
                      proplists:get_value(tcp_options, Opts, [])).

port(Port) when is_integer(Port) -> Port;
port({_Addr, Port}) -> Port.

merge_addr(Port, SockOpts) when is_integer(Port) ->
    SockOpts;
merge_addr({Addr, _Port}, SockOpts) ->
    lists:keystore(ip, 1, SockOpts, {ip, Addr}).

handle_call(options, _From, State = #state{options = Opts}) ->
    {reply, Opts, State};

handle_call(get_port, _From, State = #state{lport = LPort}) ->
    {reply, LPort, State};

handle_call(Req, _From, State) ->
    error_logger:error_msg("[~s] Unexpected call: ~p", [?MODULE, Req]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("[~s] Unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("[~s] Unexpected info: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #state{proto = Proto, listen_on = ListenOn,
                          lsock = LSock, laddr = Addr, lport = Port}) ->
    error_logger:info_msg("~s stopped on ~s~n", [Proto, esockd:format({Addr, Port})]),
    esockd_limiter:delete({listener, Proto, ListenOn}),
    esockd_server:del_stats({Proto, ListenOn}),
    esockd_transport:fast_close(LSock).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

