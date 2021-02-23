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

-module(esockd_udp).

-behaviour(gen_server).

-export([ server/4
        , count_peers/1
        , stop/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type(maybe(T) :: undefined | T).

-record(state, {
          proto       :: atom(),
          sock        :: inet:socket(),
          port        :: inet:port_number(),
          rate_limit  :: maybe(esockd_rate_limit:bucket()),
          limit_timer :: maybe(reference()),
          max_peers   :: infinity | pos_integer(),
          peers       :: map(),
          mfa         :: mfa()
         }).

-define(ACTIVE_N, 100).
-define(ENABLED(X), (X =/= undefined)).
-define(DEFAULT_OPTS, [binary, {reuseaddr, true}]).
-define(ERROR_MSG(Format, Args),
        error_logger:error_msg("[~s]: " ++ Format, [?MODULE | Args])).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec(server(atom(), esockd:listen_on(), [gen_udp:option()], mfa())
      -> {ok, pid()} | {error, term()}).
server(Proto, Port, Opts, MFA) when is_integer(Port) ->
    gen_server:start_link(?MODULE, [Proto, Port, Opts, MFA], []);
server(Proto, {Host, Port}, Opts, MFA) when is_integer(Port) ->
    IfAddr = case proplists:get_value(ip, Opts) of
                 undefined -> proplists:get_value(ifaddr, Opts);
                 Addr      -> Addr
             end,
    (IfAddr == undefined) orelse (IfAddr = Host),
    gen_server:start_link(?MODULE, [Proto, Port, merge_addr(Host, Opts), MFA], []).

merge_addr(Addr, Opts) ->
    lists:keystore(ip, 1, Opts, {ip, Addr}).

-spec(count_peers(pid()) -> integer()).
count_peers(Pid) ->
    gen_server:call(Pid, count_peers).

-spec(stop(pid()) -> ok).
stop(Pid) -> gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Proto, Port, Opts, MFA]) ->
    process_flag(trap_exit, true),
    put(incoming_peers, 0),
    {UdpOpts, State} = parse_opt(Opts, [], #state{proto = Proto,
                                                  port  = Port,
                                                  max_peers = infinity,
                                                  peers = #{},
                                                  mfa   = MFA}),
    case gen_udp:open(Port, esockd:merge_opts(?DEFAULT_OPTS, UdpOpts)) of
        {ok, Sock} ->
            %% Trigger the udp_passive event
            inet:setopts(Sock, [{active, 1}]),
            {ok, State#state{sock = Sock}};
        {error, Reason} ->
            {stop, Reason}
    end.

parse_opt([], Acc, State) ->
    {Acc, State};
parse_opt([{max_conn_rate, Rate}|Opts], Acc, State) ->
    Rl = esockd_rate_limit:new(Rate, Rate), %% Burst = Rate
    parse_opt(Opts, Acc, State#state{rate_limit = Rl});
parse_opt([{max_connections, Max}|Opts], Acc, State) ->
    parse_opt(Opts, Acc, State#state{max_peers = Max});
parse_opt([Opt|Opts], Acc, State) ->
    parse_opt(Opts, [Opt|Acc], State).

handle_call(count_peers, _From, State = #state{peers = Peers}) ->
    {reply, maps:size(Peers) div 2, State};

handle_call(Req, _From, State) ->
    ?ERROR_MSG("Unexpected call: ~p", [Req]),
    {reply, ignore, State}.

handle_cast(Msg, State) ->
    ?ERROR_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({udp, Sock, IP, InPortNo, Packet}, State = #state{sock = Sock, peers = Peers}) ->
    case maps:find(Peer = {IP, InPortNo}, Peers) of
        {ok, Pid} ->
            Pid ! {datagram, self(), Packet},
            {noreply, State};
        error ->
            put(incoming_peers, get(incoming_peers) + 1),
            try should_throttle(State) orelse
                start_channel({udp, self(), Sock}, Peer, State) of
                true ->
                    ?ERROR_MSG("Cannot create udp channel for peer ~s due to throttling.",
                               [esockd:format(Peer)]),
                    {noreply, State};
                {ok, Pid} ->
                    _Ref = erlang:monitor(process, Pid),
                    Pid ! {datagram, self(), Packet},
                    {noreply, store_peer(Peer, Pid, State)};
                {error, Reason} ->
                    ?ERROR_MSG("Failed to start udp channel for peer ~s, reason: ~p",
                               [esockd:format(Peer), Reason]),
                    {noreply, State}
            catch
                _Error:Reason ->
                    ?ERROR_MSG("Exception occurred when starting udp channel for peer ~s, reason: ~p",
                               [esockd:format(Peer), Reason]),
                    {noreply, State}
            end
    end;

handle_info({udp_passive, Sock}, State = #state{sock = Sock, rate_limit = Rl}) ->
    NState = case ?ENABLED(Rl) andalso
                  esockd_rate_limit:check(put(incoming_peers, 0), Rl) of
                 false ->
                     activate_sock(State);
                 {0, Rl1} ->
                     activate_sock(State#state{rate_limit = Rl1});
                 {Pause, Rl1} ->
                     ?ERROR_MSG("Pause ~w(ms) due to rate limit.", [Pause]),
                     TRef = erlang:start_timer(Pause, self(), activate_sock),
                     State#state{rate_limit = Rl1, limit_timer = TRef}
             end,
    {noreply, NState, hibernate};

handle_info({timeout, TRef, activate_sock}, State = #state{limit_timer = TRef}) ->
    NState = State#state{limit_timer = undefined},
    {noreply, activate_sock(NState)};

handle_info({'DOWN', _MRef, process, DownPid, _Reason}, State = #state{peers = Peers}) ->
    case maps:find(DownPid, Peers) of
        {ok, Peer} ->
            {noreply, erase_peer(Peer, DownPid, State)};
        error -> {noreply, State}
    end;

handle_info({datagram, Peer = {IP, Port}, Packet}, State = #state{sock = Sock}) ->
    case gen_udp:send(Sock, IP, Port, Packet) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR_MSG("Dropped packet to: ~s, reason: ~s", [esockd:format(Peer), Reason])
    end,
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{sock = Sock}) ->
    gen_udp:close(Sock).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internel functions
%%--------------------------------------------------------------------

-compile({inline,
          [ should_throttle/1
          , start_channel/3
          , activate_sock/1
          , store_peer/3
          , erase_peer/3
          ]}).

should_throttle(#state{max_peers = infinity}) -> false;
should_throttle(#state{max_peers = MaxLimit, peers = Peers}) ->
    (maps:size(Peers) div 2) > MaxLimit.

start_channel(Transport, Peer, #state{mfa = {M, F, Args}}) ->
    erlang:apply(M, F, [Transport, Peer | Args]).

activate_sock(State = #state{sock = Sock}) ->
    inet:setopts(Sock, [{active, ?ACTIVE_N}]), State.

store_peer(Peer, Pid, State = #state{peers = Peers}) ->
    State#state{peers = maps:put(Pid, Peer, maps:put(Peer, Pid, Peers))}.

erase_peer(Peer, Pid, State = #state{peers = Peers}) ->
    State#state{peers = maps:remove(Peer, maps:remove(Pid, Peers))}.

