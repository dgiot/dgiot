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
-module(dgiot_mqtt_client).
-author("zwx").
-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([start_link/3, subscribe/3, publish/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {options, conn = disconnect, props, mod, childState}).

%%%===================================================================
%%% API
%%%===================================================================

subscribe(Client, Topic, Qos) ->
    gen_server:call(Client, {subscribe, Topic, Qos}).

publish(Client, Topic, Payload, Qos) ->
    gen_server:call(Client, {publish, Topic, Payload, Qos}).

start_link(Mod, Init, Options) ->
    gen_server:start_link(?MODULE, [Mod, Init, format(Options)], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Mod, Init, Options]) ->
    case Mod:init(Init) of
        {ok, ChildState} ->
            process_flag(trap_exit, true),
            self() ! connect,
            {ok, #state{
                mod = Mod,
                options = Options,
                childState = ChildState
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({publish, Topic, Payload, Qos}, _From, #state{conn = ConnPid} = State) ->
    case ConnPid of
        disconnect ->
            {reply, {error, disconnect}, State};
        Pid ->
            Reply = emqtt:publish(Pid, Topic, Payload, Qos),
            {reply, Reply, State}
    end;

handle_call({subscribe, Topic, Qos}, _From, #state{conn = ConnPid} = State) ->
    case ConnPid of
        disconnect ->
            {reply, {error, disconnect}, State};
        Pid ->
            Reply = emqtt:subscribe(Pid, {Topic, Qos}),
            {reply, Reply, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(connect, #state{options = Options, mod = Mod, childState = ChildState} = State) ->
    case connect(Options) of
        {ok, ConnPid, Props} ->
            case Mod:handle_info({connect, ConnPid}, ChildState) of
                {noreply, NewChildState} ->
                    {noreply, State#state{options = Options, childState = NewChildState, conn = ConnPid, props = Props}};
                {stop, Reason, NewChildState} ->
                    {stop, Reason, State#state{options = Options, childState = NewChildState, conn = ConnPid, props = Props}}
            end;
        {error, unauthorized} ->
            ?LOG(warning,"connect ~p", [unauthorized]),
            {stop, normal, State};
        {error, econnrefused} ->
            {noreply, State#state{conn = disconnect, props = undefined}};
        {error, Reason} ->
            ?LOG(warning,"connect error,~p", [Reason]),
            {noreply, State#state{conn = disconnect, props = undefined}}
    end;

handle_info({publish, Message}, #state{mod = Mod, childState = ChildState} = State) ->
    case Mod:handle_info({publish, Message}, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{childState = NewChildState}};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{childState = NewChildState}}
    end;

handle_info({disconnected,shutdown,tcp_closed}, State) ->
    {noreply, State#state{ conn = disconnect}};
handle_info({'EXIT', _Conn, Reason}, #state{mod = Mod, childState = ChildState} = State) ->
    case Mod:handle_info(disconnect, ChildState) of
        {noreply, NewChildState} ->
            Sec = application:get_env(dgiot_framework, reconnect, 10),
            case erlang:system_time(second) - get(last) > Sec of
                true ->
                    self() ! connect;
                false ->
                    erlang:send_after(Sec * 1000, self(), connect)
            end,
            {noreply, State#state{childState = NewChildState, conn = disconnect}};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{childState = NewChildState, conn = disconnect}}
    end;

handle_info(Info, #state{mod = Mod, childState = ChildState} = State) ->
    case Mod:handle_info(Info, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{childState = NewChildState}};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{childState = NewChildState, conn = disconnect}}
    end.

terminate(Reason, #state{mod = Mod, childState = ChildState}) ->
    Mod:terminate(Reason, ChildState).

code_change(OldVsn, #state{mod = Mod, childState = ChildState} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#state{childState = NewChildState}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(Options) ->
    put(last, erlang:system_time(second)),
    case emqtt:start_link([{owner, self()} | Options]) of
        {ok, ConnPid} ->
            case catch emqtt:connect(ConnPid) of
                {ok, Props} ->
                    {ok, ConnPid, Props};
                {error, {unauthorized_client,_}} ->
                    {error, unauthorized};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

format(Opts) ->
    lists:foldl(
        fun(Fun, Acc) ->
            Fun(Acc)
        end, Opts, [fun format_ssl/1, fun format_clean_start/1]).

format_ssl(Opts) ->
    case proplists:get_value(ssl, Opts) of
        true ->
            case proplists:get_value(ssl_opts, Opts) of
                undefined ->
                    [{ssl, false} | proplists:delete(ssl, Opts)];
                _ ->
                    Opts
            end;
        _ ->
            [{ssl, false} | proplists:delete(ssl, Opts)]
    end.

format_clean_start(Opts) ->
    case proplists:get_value(clean_start, Opts) of
        true ->
            Opts;
        _ ->
            [{clean_start, false} | proplists:delete(clean_start, Opts)]
    end.
