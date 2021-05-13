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

-module(ekka_cluster_mcast).

-behaviour(gen_server).

-behaviour(ekka_cluster_strategy).

%% Cluster strategy callbacks
-export([ discover/1
        , lock/1
        , unlock/1
        , register/1
        , unregister/1
        ]).

-export([ ensure_started/1
        , start_link/1
        , stop/0
        ]).

%% For tests
-export([get_sock/0]).

%% gen_server Callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-import(proplists, [get_value/2, get_value/3]).

-type(option() :: {addr, inet:ip_address()}
                | {ports, list(inet:port_number())}
                | {iface, inet:ip_address()}
                | {ttl, pos_integer()}
                | {loop, boolean()}
                | {senbuf, pos_integer()}
                | {recbuf, pos_integer()}
                | {buffer, pos_integer()}).

-record(state, {
          sock   :: inet:socket(),
          addr   :: inet:ip_address(),
          ports  :: list(inet:port_number()),
          cookie :: integer(),
          seen   :: list(node())
         }).

-define(SERVER, ?MODULE).

-define(LOG(Level, Format, Args),
        logger:Level("Ekka(Mcast): " ++ Format, Args)).

%%--------------------------------------------------------------------
%% ekka_cluster_strategy callbacks
%%--------------------------------------------------------------------

discover(Options) ->
    Server = case whereis(?SERVER) of
                 Pid when is_pid(Pid) -> Pid;
                 undefined -> ensure_started(Options)
             end,
    gen_server:call(Server, discover, 60000).

lock(_Options) ->
    ignore.

unlock(_Options) ->
    ignore.

register(_Options) ->
    ignore.

unregister(_Options) ->
    ignore.

%%--------------------------------------------------------------------
%% Start/stop mcast server
%%--------------------------------------------------------------------

ensure_started(Options) ->
    case ekka_cluster_sup:start_child(?SERVER, [Options]) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

-spec(start_link(list(option())) -> {ok, pid()} | {error, term()}).
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

-spec(stop() -> ok).
stop() -> gen_server:stop(?SERVER).

-spec(get_sock() -> inet:socket()).
get_sock() ->
    gen_server:call(?SERVER, get_sock).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Options) ->
    Addr  = get_value(addr, Options),
    Ports = get_value(ports, Options),
    Loop  = get_value(loop, Options, true),
    TTL   = get_value(ttl, Options, 1),
    Iface = get_value(iface, Options, {0,0,0,0}),
    case udp_open(Ports, [{multicast_if, Iface},
                          {multicast_ttl, TTL},
                          {multicast_loop, Loop},
                          {add_membership, {Addr, Iface}}]) of
        {ok, Sock} ->
            Cookie = erlang:phash2(erlang:get_cookie()),
            {ok, #state{sock = Sock, addr = Addr, ports = Ports,
                        cookie = Cookie, seen = []}};
        {error, Error} -> {stop, Error}
    end.

handle_call(discover, From, State = #state{sock = Sock, addr = Addr,
                                           ports = Ports, cookie = Cookie}) ->
    lists:foreach(fun(Port) ->
                      udp_send(Sock, Addr, Port, ping(Cookie))
                  end, Ports),
    erlang:send_after(3000, self(), {reply, discover, From}),
    {noreply, State};

handle_call(get_sock, _From, State = #state{sock = Sock}) ->
    {reply, Sock, State};

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Req]),
    {reply, ignore, State}.

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({reply, discover, From}, State = #state{seen = Seen}) ->
    gen_server:reply(From, {ok, [node() | Seen]}),
    {noreply, State#state{seen = []}, hibernate};

handle_info({udp, Sock, _Ip, InPort, Data},
            State = #state{sock = Sock, addr = Addr,
                           cookie = Cookie, seen = Seen}) ->
    %%io:format("~s recv handshake from ~p: ~p~n",
    %%          [node(), {Ip, InPort}, binary_to_term(Data)]),
    Cluster = ekka:env(cluster_name, ekka),
    {noreply, try binary_to_term(Data) of
                  {ping, Node, _Cluster, _Cookie} when Node =:= node() ->
                      State;
                  {ping, Node, Cluster, Cookie} ->
                      udp_send(Sock, Addr, InPort, pong(Cookie)),
                      State#state{seen = lists:usort([Node | Seen])};
                  {pong, Node, _Cluster, _Cookie} when Node =:= node() ->
                      State;
                  {pong, Node, Cluster, Cookie} ->
                      State#state{seen = lists:usort([Node | Seen])};
                  Handshake = {_Type, _Node, _Cluster, _Cookie} ->
                       ?LOG(error, "Bad handshake: ~p", [Handshake]),
                       State;
                  Term -> ?LOG(error, "Bad term: ~p", [Term]),
                          State
              catch
                  error:badarg ->
                      ?LOG(error, "Corrupt data: ~p", [Data]),
                      State
              end, hibernate};

handle_info({udp_passive, Sock}, State = #state{sock = Sock}) ->
    inet:setopts(Sock, [{active, 10}]),
    {noreply, State};

handle_info({udp_closed, Sock}, State = #state{sock = Sock}) ->
    {stop, udp_closed, State};

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{sock = Sock}) ->
    gen_udp:close(Sock).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ping(Cookie) -> handshake(ping, Cookie).

pong(Cookie) -> handshake(pong, Cookie).

handshake(Type, Cookie) ->
    {Type, node(), ekka:env(cluster_name, undefined), Cookie}.

udp_open([], _Options) ->
    {error, eaddrinuse};

udp_open([Port|Ports], Options) ->
    case gen_udp:open(Port, [binary, {active, 10}, {reuseaddr, true} | Options]) of
        {ok, Sock} ->
            {ok, Sock};
        {error, eaddrinuse} ->
            ?LOG(warning, "Multicast Adddress inuse: ~p", [Port]),
            udp_open(Ports, Options);
        {error, Reason} ->
            {error, Reason}
    end.

udp_send(Sock, Addr, Port, Term) ->
    gen_udp:send(Sock, Addr, Port, term_to_binary(Term)).

