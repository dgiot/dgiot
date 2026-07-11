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


-module(dgiot_channelx).
-author("johnliu").
-define(CHANNEL(Type, Id), binary_to_atom(get_name(ChannelType, ChannelId), utf8)).
-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    get_name/1, get_name/2,
    add2/5, add/4, add/5,
    spec/0, spec/1,
    delete/2, delete/3,
    status/1,
    do_event/3, do_event/4, do_event/5, send_after/3,
    do_message/2, do_message/3, do_message/4,
    call/3, call/4, call2/3, call2/4,
    start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {childState, mod}).

-callback init(ChannelType :: any(), ChannelId :: any(), ChannelArgs :: map()) ->
    {ok, State :: any()} |
    {ok, State :: any(), ChildSpec :: {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.

-callback handle_message(Message :: map(), State :: any()) ->
    ok | {ok, Reply :: any(), State :: any()} | {ok, State :: any()} | {error, Reason :: any()}.

-callback handle_event(EventType :: atom(), Event :: map(), State :: any()) ->
    ok | {ok, State :: any()} | {error, Reason :: any()}.

-callback stop(ChannelType :: any(), ChannelId :: any(), State :: any()) -> ok.

%%%===================================================================
%%% API
%%%===================================================================

add2(ServerName, ChannelType, ChannelId, Mod, ChannelArgs) ->
    dgiot_channelx_sup:add(channelx_mgr, ServerName, ChannelType, ChannelId, Mod, ChannelArgs).

add(ChannelType, ChannelId, Mod, ChannelArgs) ->
    CType1 = list_to_binary(string:uppercase(dgiot_utils:to_list(ChannelType))),
    add(channelx_mgr, CType1, ChannelId, Mod, ChannelArgs).

add(Sup, ChannelType, ChannelId, Mod, ChannelArgs) ->
    dgiot_channelx_sup:add(Sup, ChannelType, ChannelId, Mod, ChannelArgs).


delete(ChannelType, ChannelId) ->
    delete(channelx_mgr, ChannelType, ChannelId).

delete(Sup, ChannelType, ChannelId) ->
    dgiot_channelx_sup:delete(Sup, ChannelType, ChannelId).

spec() ->
    spec(channelx_mgr).


spec(SerName) ->
    {dgiot_channelx_mgr, {dgiot_channelx_mgr, start_link, [SerName]}, permanent, 5000, supervisor, [dgiot_channelx_mgr]}.

status(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, status);
status(Id) ->
    gen_statem:call(name(Id), status).

name(Id) -> list_to_atom(lists:concat([?MODULE, "_", Id])).

do_event(ChannelId, EventId, Event) ->
    case dgiot_data:get({channeltype, ChannelId}) of
        not_find ->
            not_find;
        ChannelType ->
            do_event(ChannelType, ChannelId, EventId, Event, 5000)
    end.

do_event(ChannelType, ChannelId, EventId, Event) ->
    do_event(ChannelType, ChannelId, EventId, Event, 5000).
do_event(ChannelType, ChannelId, EventId, Event, Timeout) ->
    Pool = ?CHANNEL(ChannelType, ChannelId),
    Fun =
        fun(Worker) ->
            Worker ! {event, Pool, EventId, Event}, ok
        end,
%%    ?LOG(error, "EventId ~p", [EventId]),
    poolboy:transaction(Pool, Fun, Timeout).


send_after(Time, ChannelId, Message) ->
    case dgiot_data:get({channeltype, ChannelId}) of
        not_find ->
            not_find;
        ChannelType ->
            Pool = ?CHANNEL(ChannelType, ChannelId),
            Fun =
                fun(Worker) ->
                    erlang:send_after(Time, Worker, {message, Pool, Message}),
                    ok
                end,
            poolboy:transaction(Pool, Fun, 5000)
    end.

do_message(ChannelId, Message) ->
    case dgiot_data:get({channeltype, ChannelId}) of
        not_find ->
            not_find;
        ChannelType ->
            do_message(ChannelType, ChannelId, Message, 5000)
    end.

do_message(ChannelType, ChannelId, Message) ->
    do_message(ChannelType, ChannelId, Message, 5000).
do_message(ChannelType, ChannelId, Message, Timeout) ->
    Pool = ?CHANNEL(ChannelType, ChannelId),
    Fun =
        fun(Worker) ->
            Worker ! {message, Pool, Message}, ok
        end,
    poolboy:transaction(Pool, Fun, Timeout).

call(ChannelType, ChannelId, Request) ->
    call(ChannelType, ChannelId, Request, 5000).
call(ChannelType, ChannelId, Request, Timeout) ->
    Pool = ?CHANNEL(ChannelType, ChannelId),
    Fun =
        fun(Worker) ->
            gen_server:call(Worker, Request, Timeout)
        end,
    try
        poolboy:transaction(Pool, Fun, Timeout)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            {error, not_find_channel};
        _Type:Reason ->
            {error, Reason}
    end.

call2(ChannelType, ChannelId, Request) ->
    call2(ChannelType, ChannelId, Request, 5000).
call2(ChannelType, ServerName, Request, Timeout) ->
    case dgiot_data:lookup({ServerName, channel2}) of
        {ok, ChannelId} ->
            call(ChannelType, ChannelId, Request, Timeout);
        {error, not_find} ->
            call(ChannelType, ServerName, Request, Timeout)
    end.

get_name(ChannelId) ->
    case dgiot_data:get({channeltype, ChannelId}) of
        not_find ->
            pass;
        ChannelType ->
            get_name(binary_to_list(ChannelType), ChannelId)
    end.

get_name(ChannelType, ChannelId) when is_binary(ChannelType) ->
    get_name(binary_to_list(ChannelType), ChannelId);
get_name(ChannelType, ChannelId) when is_binary(ChannelId) ->
    get_name(ChannelType, binary_to_list(ChannelId));
get_name(ChannelType, ChannelId) ->
    list_to_binary(lists:concat([ChannelType, "/", ChannelId])).

start_link([Mod, State]) ->
    gen_server:start_link(?MODULE, [Mod, State], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Mod, ChildState]) ->
    case erlang:module_loaded(Mod) of
        true ->
            case erlang:function_exported(Mod, handle_init, 1) of
                true ->
                    case Mod:handle_init(ChildState) of
                        {ok, NewChildState} ->
                            {ok, #state{childState = NewChildState, mod = Mod}};
                        {stop, Reason} ->
                            {stop, Reason}
                    end;
                false ->
                    {ok, #state{childState = ChildState, mod = Mod}}
            end;
        false ->
            {stop, <<"module not exist">>}
    end.


handle_call(Request, _From, State) ->
    case catch handle_message(Request, State) of
        ok ->
            {reply, ok, State};
        {reply, Reply, NewState} ->
            {reply, Reply, State#state{childState = NewState}};
        {ok, NewState} ->
            {reply, ok, State#state{childState = NewState}};
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            {reply, {error, Reason}, State}
    end.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({event, _Pool, EventId, Event}, State) ->
    Result = handle_event(EventId, Event, State),
    case Result of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, State#state{childState = NewState}};
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "do_event, ~p, ~p,~p", [EventId, Event, Reason]),
            {noreply, State}
    end;

handle_info({message, _Pool, Message}, State) ->
    Result = handle_message(Message, State),
    case Result of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, State#state{childState = NewState}};
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "do_message, ~p,~p", [Message, Reason]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    Result = handle_message(Info, State),
    case Result of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, State#state{childState = NewState}};
        {stop, Reason} ->
            {stop, Reason, State};
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "do_message, ~p,~p", [Info, Reason]),
            {noreply, State};
        _ ->
            {noreply, State}
    end.

terminate(Reason, _State) ->
    ?LOG(error, "~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_event(register, Device, #state{mod = Mod, childState = ChildState} = State) ->
    case Mod:handle_event(register, Device, ChildState) of
        ok ->
            handle_event(online, Device, State);
        {ok, NewChildState} ->
            handle_event(online, Device, State#state{childState = NewChildState});
        {error, Reason} ->
            {error, Reason}
    end;

handle_event(online, Device, #state{mod = Mod, childState = State}) ->
    Mod:handle_event(online, Device, State);

handle_event(offline, Event, #state{mod = Mod, childState = State}) ->
    Mod:handle_event(offline, Event, State);

handle_event(EventId, Event, #state{mod = Mod, childState = State}) ->
    Mod:handle_event(EventId, Event, State).


handle_message(Message, #state{mod = Mod, childState = State}) ->
    Mod:handle_message(Message, State).
