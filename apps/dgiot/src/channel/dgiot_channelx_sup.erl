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

-module(dgiot_channelx_sup).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-behaviour(supervisor).

%% API
-export([start_link/4, add/5, add/6,  delete/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {childState, workerState, server, mod, type, id}).


add(Sup, ServerName, ChannelType, ChannelId, Mod, ChannelArgs) ->
    Name = binary_to_atom(dgiot_channelx:get_name(ChannelType, ChannelId), utf8),
    case dgiot_data:lookup({Name, channel}) of
        {ok, {Pid, _OldArgs}} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    do_start(Sup, ServerName, ChannelType, ChannelId, Mod, ChannelArgs)
            end;
        _ ->
            do_start(Sup, ServerName, ChannelType, ChannelId, Mod, ChannelArgs)
    end.

add(Sup, ChannelType, ChannelId, Mod, ChannelArgs) ->
    add(Sup, undefined, ChannelType, ChannelId, Mod, ChannelArgs).

do_start(Sup, ServerName, ChannelType, ChannelId, Mod, ChannelArgs) ->
    Name = binary_to_atom(dgiot_channelx:get_name(ChannelType, ChannelId), utf8),
    case whereis(Name) of
        undefined -> ok;
        Pid -> exit(Pid, normal)
    end,
    dgiot_data:insert({Name, channel}, {undefined, ChannelArgs}),
    case supervisor:start_child(Sup, [ServerName, ChannelType, ChannelId, Mod]) of
        {ok, Pid1} ->
            {ok, Pid1};
        {error, Reason} ->
            dgiot_data:delete({Name, channel}),
            {error, Reason}
    end.

delete(Sup, ChannelType, ChannelId) ->
    Name = binary_to_atom(dgiot_channelx:get_name(ChannelType, ChannelId), utf8),
    case dgiot_data:lookup({Name, channel}) of
        {ok, {Pid, _}} ->
            case whereis(Sup) of
                undefined ->
                    dgiot_data:delete({Name, channel});
                _ ->
                    case supervisor:terminate_child(Sup, Pid) of
                        ok ->
                            dgiot_data:delete({Name, channel});
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.


start_link(ServerName, ChannelType, ChannelId, Mod) ->
    gen_server:start_link(?MODULE, [sup, ServerName, ChannelType, ChannelId, Mod], []).


init([sup, ServerName, ChannelType, ChannelId, Mod]) ->
    case supervisor:init({self, ?MODULE, [ChannelType, ChannelId, Mod]}) of
        {ok, State} ->
            WorkerState = erase(worker_state),
            dgiot_data:insert({ServerName, channel2}, ChannelId),
            {ok, #state{
                server = ServerName,
                childState = State,
                workerState = WorkerState,
                mod = Mod,
                type = ChannelType,
                id = ChannelId
            }};
        ignore ->
            ignore;
        {stop,{shutdown,Reason}} ->
            {stop, Reason};
        {stop, {bad_return, {?MODULE, init, Reason}}} ->
            {stop, Reason};
        {stop, {bad_start_spec, Reason}} ->
            {stop, {bad_start_spec, Reason}};
        {stop, {start_spec, Reason}} ->
            {stop, {start_spec, Reason}};
        {stop, {supervisor_data, Reason}} ->
            {stop, Reason};
        {stop, Reason} ->
            {stop, Reason}
    end;


init([ChannelType, ChannelId, Mod]) ->
    Name = binary_to_atom(dgiot_channelx:get_name(ChannelType, ChannelId), utf8),
    ?LOG(error,"Name ~p",[Name]),
    case dgiot_data:lookup({Name, channel}) of
        {ok, {_, ChannelArgs}} ->
            Size = maps:get(<<"Size">>, ChannelArgs, 5),
            MaxOverFlow = maps:get(<<"MaxOverFlow">>, ChannelArgs, 10),
            PoolArgs = [
                {name, {local, Name}},
                {size, Size},
                {max_overflow, MaxOverFlow},
                {worker_module, dgiot_channelx}
            ],
            Result =
                case Mod:init(ChannelType, ChannelId, ChannelArgs) of
                    {stop, Why} ->
                        {stop, Why};
                    {ok, State} ->
                        put(worker_state, State),
                        {ok, [
                            poolboy:child_spec(Name, PoolArgs, [Mod, State])
                        ]};
                    {ok, State, ChildSpec} ->
                        put(worker_state, State),
                        case is_list(ChildSpec) of
                            true ->
                                {ok, [
                                    poolboy:child_spec(Name, PoolArgs, [Mod, State]) | ChildSpec
                                ]};
                            false ->
                                {ok, [
                                    poolboy:child_spec(Name, PoolArgs, [Mod, State]),
                                    ChildSpec
                                ]}
                        end
                end,
            case Result of
                {ok, Children} ->
                    dgiot_data:insert({Name, channel}, {self(), ChannelArgs}),
                    dgiot_mqtt:subscribe(<<"channel/", ChannelId/binary>>),
                    {ok, {{one_for_all, 1000, 3600}, Children}};
                {stop, Reason} ->
                    Reason
            end;
        _ ->
            not_find_args
    end.


handle_call(Request, From, #state{childState = ChildState} = State) ->
    {reply, Reply, NewState} = supervisor:handle_call(Request, From, ChildState),
    {reply, Reply, State#state{childState = NewState}}.

handle_cast(Msg, #state{childState = ChildState} = State) ->
    case supervisor:handle_cast(Msg, ChildState) of
        {noreply, NewState} ->
            {noreply, State#state{childState = NewState}};
        {stop, shutdown, NewState} ->
            {stop, shutdown, State#state{childState = NewState}}
    end.

handle_info(Info, #state{childState = ChildState} = State) ->
    case supervisor:handle_info(Info, ChildState) of
        {noreply, NewState} ->
            {noreply, State#state{childState = NewState}};
        {stop, shutdown, NewState} ->
            {stop, shutdown, State#state{childState = NewState}}
    end.

terminate(Reason, #state{childState = ChildState, mod = Mod, type = Type, id = Id} = State) ->
    dgiot_data:delete({State#state.server, channel2}),
    case erlang:function_exported(Mod, stop, 3) of
        true ->
            Mod:stop(Type, Id, State#state.workerState);
        false ->
            Mod:stop(Type, Id)
    end,
    supervisor:terminate(Reason, ChildState).

code_change(OldVsn, State, Extra) ->
    case supervisor:code_change(OldVsn, State, Extra) of
        {ok, NewState} ->
            {ok, State#state{childState = NewState}};
        {error, Reason} ->
            {error, Reason}
    end.

