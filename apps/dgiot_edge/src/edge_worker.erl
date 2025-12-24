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
-module(edge_worker).
-author("johnliu").
-include("dgiot_edge.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#{<<"serialport">> := Serialport} = State) ->
    case dgiot_data:lookup({edge_task, Serialport}) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    gen_server:start_link(?MODULE, [State], [])
            end;
        _Reason ->
            gen_server:start_link(?MODULE, [State], [])
    end;

start_link(_State) ->
    ok.

stop(Serialport) ->
    case dgiot_data:lookup({edge_task, Serialport}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"serialport">> := Serialport, <<"frequency">> := Freq, <<"messagetype">> := Messagetype, <<"data">> := Data} = _Args]) ->
%%    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, _Args]),
    dgiot_data:insert({edge_task, Serialport}, self()),
    NewData = dgiot_edge:get_writeData(Messagetype, Data),
    erlang:send_after(1000, self(), write_serialport),
    {ok, #task{freq = dgiot_utils:to_int(Freq), serialport = Serialport, data = NewData}};

init(A) ->
    ?LOG(info, "A ~p ", [A]).

handle_call(stop, _From, State) ->
    erlang:garbage_collect(self()),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    erlang:garbage_collect(self()),
    {stop, Reason, State};

handle_info(write_serialport, #task{freq = Freq, serialport = Serialport, data = Data} = State) ->
    case dgiot_serial_client:write(self(), Serialport, Data) of
        pass ->
            erlang:garbage_collect(self()),
            {stop, normal, State};
        _ ->
            erlang:send_after(Freq, self(), write_serialport),
            {noreply, State}
    end;

handle_info({deliver, _, _Msg}, State) ->
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



