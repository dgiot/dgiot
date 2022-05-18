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

-module(dgiot_tcp_client).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).
%% API
-export([start_link/6, start_link/4, start_link/5, start_link/3, start_link/7, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {host, port, child = #tcp{}, mod, reconnect_times, reconnect_sleep = 30}).
-define(TIMEOUT, 10000).
-define(TCP_OPTIONS, [binary, {active, once}, {packet, raw}, {reuseaddr, false}, {send_timeout, ?TIMEOUT}]).


start_link(Mod, Host, Port) ->
    start_link(Mod, Host, Port, undefined).

start_link(Mod, Host, Port, Args) ->
    start_link(Mod, Host, Port, max, 30, Args).

start_link(Name, Mod, Host, Port, Args) ->
    start_link(Name, Mod, Host, Port, max, 30, Args).

start_link(Mod, Host, Port, ReconnectTimes, ReconnectSleep, Args) ->
    start_link(undefined, Mod, Host, Port, ReconnectTimes, ReconnectSleep, Args).

start_link(Name, Mod, Host, Port, ReconnectTimes, ReconnectSleep, Args) ->
    Ip = dgiot_utils:to_list(Host),
    Port1 = dgiot_utils:to_int(Port),
    State = #state{
        mod = Mod,
        host = Ip,
        port = Port1,
        reconnect_times = ReconnectTimes, %% 重连次数
        reconnect_sleep = ReconnectSleep  %% 重连间隔
    },
    case Name of
        undefined ->
            gen_server:start_link(?MODULE, [State, Args], []);
        _ ->
            gen_server:start_link({local, list_to_atom(dgiot_utils:to_list(Name))}, ?MODULE, [State, Args], [])
    end.


init([#state{mod = Mod} = State, Args]) ->
    ?LOG(info,"Mod ~p Args ~p ",[Mod, Args]),
    Transport = gen_tcp,
    Child = #tcp{transport = Transport, socket = undefined},
    case Mod:init(Child#tcp{state = Args}) of
        {ok, ChildState} ->
            NewState = State#state{
                child = ChildState
            },
            {ok, do_connect(false, NewState), hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({connection_ready, Socket}, _From, #state{mod = Mod, child = ChildState} = State) ->
    NewChildState = ChildState#tcp{socket = Socket},
    case Mod:handle_info(connection_ready, NewChildState) of
        {noreply, NewChildState1} ->
            {reply, ok, State#state{child = NewChildState1}, hibernate};
        {stop, Reason, NewChildState1} ->
            {stop, Reason, {error, Reason}, State#state{child = NewChildState1}}
    end;

handle_call(Request, From, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_call(Request, From, ChildState) of
        {reply, Reply, NewChildState} ->
            {reply, Reply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end.

handle_cast(Msg, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_cast(Msg, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end.

%% 连接次数为0了
handle_info(do_connect, State) ->
    ?LOG(info,"CONNECT CLOSE ~s:~p", [State#state.host, State#state.port]),
    {stop, normal, State};

%% 连接次数为0了
handle_info(connect_stop, State) ->
    ?LOG(info,"CONNECT CLOSE ~s:~p", [State#state.host, State#state.port]),
    {stop, normal, State};

handle_info({connection_ready, Socket}, #state{mod = Mod, child = ChildState} = State) ->
    NewChildState = ChildState#tcp{socket = Socket},
    ?LOG(info,"connection_ready ~p~n", [Socket]),
    case Mod:handle_info(connection_ready, NewChildState) of
        {noreply, NewChildState1} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{child = NewChildState1}, hibernate};
        {stop, Reason, NewChildState1} ->
            {stop, Reason, State#state{child = NewChildState1}}
    end;

handle_info({tcp, Socket, Binary}, State) ->
%%    ?LOG(info,"Binary ~p~n", [Binary]),
    #state{mod = Mod, child = #tcp{socket = Socket} = ChildState} = State,
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    case Mod:handle_info({tcp, NewBin}, ChildState) of
        {noreply, NewChildState} ->
            inet:setopts(ChildState#tcp.socket, [{active, once}]),
            {noreply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end;

handle_info({tcp_error, _Socket, _Reason}, #state{child = _ChildState} = State) ->
    {noreply, State, hibernate};

handle_info({Closed, _Sock}, #state{mod = Mod, child = #tcp{transport = Transport, socket = Socket} = ChildState} = State) when Closed == tcp_closed ->
    Transport:close(Socket),
    case Mod:handle_info(Closed, ChildState) of
        {noreply, NewChildState} ->
            NewState = State#state{child = NewChildState#tcp{socket = undefined}},
            case is_integer(NewState#state.reconnect_sleep) of
                false ->
                    {stop, normal, NewState};
                true ->
                    Now = erlang:system_time(second),
                    Sleep =
                        case get(last_closed) of
                            Time when is_integer(Time) andalso Now - Time < State#state.reconnect_sleep ->
                                true;
                            _ ->
                                false
                        end,
                    put(last_closed, Now),
                    {noreply, do_connect(Sleep, NewState), hibernate}
            end;
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState#tcp{socket = undefined}}}
    end;

handle_info(Info, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_info(Info, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end.

terminate(Reason, #state{mod = Mod, child = ChildState}) ->
    Mod:terminate(Reason, ChildState).

code_change(OldVsn, #state{mod = Mod, child = ChildState} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#state{child = NewChildState}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
send(#tcp{transport = Transport, socket = Socket, log = _Log}, Payload) ->
    case Socket == undefined of
        true ->
            {error, disconnected};
        false ->
            timer:sleep(1),
            Transport:send(Socket, Payload)
    end.

do_connect(Sleep, #state{child = TCPState} = State) ->
    Client = self(),
    NewState = State#state{
        child = TCPState#tcp{
            socket = undefined
        }
    },
    spawn(
        fun() ->
            Sleep andalso timer:sleep(State#state.reconnect_sleep * 1000),
            connect(Client, NewState)
        end),
    NewState.

connect(Client, #state{host = Host, port = Port, reconnect_times = Times, reconnect_sleep = Sleep} = State) ->
    case is_process_alive(Client) of
        true ->
            case gen_tcp:connect(Host, Port, ?TCP_OPTIONS, ?TIMEOUT) of
                {ok, Socket} ->
                    case catch gen_server:call(Client, {connection_ready, Socket}, 5000) of
                        ok ->
                            inet:setopts(Socket, [{active, once}]),
                            gen_tcp:controlling_process(Socket, Client);
                        _ ->
                            ok
                    end;
                {error, Reason} ->
                    case is_integer(Times) of
                        true when Times - 1 > 0 ->
                            Client ! {connection_error, Reason},
                            timer:sleep(Sleep * 1000),
                            connect(Client, State#state{reconnect_times = Times - 1});
                        false when is_atom(Times) ->
                            Client ! {connection_error, Reason},
                            timer:sleep(Sleep * 1000),
                            connect(Client, State);
                        _ ->
                            Client ! connect_stop
                    end
            end;
        false ->
            ok
    end.

