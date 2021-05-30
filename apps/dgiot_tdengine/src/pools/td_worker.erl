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

-module(td_worker).
-author("johnliu").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(task, {productid, channelid, key, status = offline}).
%%%===================================================================
%%% API
%%%===================================================================
start_link(#{<<"channelid">> := ChannelId, <<"key">> := Key} = State) ->
    case dgiot_data:lookup(?DGIOT_TD_CH_WORK, {ChannelId, Key}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    gen_server:start_link(?MODULE, [State], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"productid">> := ProductId, <<"channelid">> := ChannelId, <<"key">> := Key}]) ->
    dgiot_data:insert(?DGIOT_TD_CH_WORK, {ChannelId, Key}, self()),
    SubTopic = <<"dgtd_", Key/binary,"/", ChannelId/binary>>,
    dgiot_mqtt:subscribe(SubTopic),
    ?LOG(info,"SubTopic ~p ",[SubTopic]),
    erlang:send_after(1000, self(), connect_td),
    {ok, #task{productid = ProductId, channelid = ChannelId, key = Key, status = online}};

init(A) ->
    io:format("A ~p ",[A]).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(disconnect, _From, State) ->
    ?LOG(info,"State ~p",[State]),
    {reply, noreply, State#task{status = offline}};

handle_call({debug, Sql}, _From, #task{key = Key} = State) ->
    PubTopic = <<"dgtd_", Key/binary,"/debug">>,
    dgiot_mqtt:publish(?MODULE, PubTopic, Sql),
    ?LOG(info,"PubTopic ~p Sql ~p",[PubTopic, Sql]),
    {reply, noreply, State};

handle_call({sql, Sql}, _From, #task{key = Key} = State) ->
    PubTopic = <<"dgtd_", Key/binary,"/sql">>,
    dgiot_mqtt:publish(?MODULE, PubTopic, Sql),
    ?LOG(info,"PubTopic ~p Sql ~p",[PubTopic, Sql]),
    {reply, noreply, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info({deliver, _Topic, Msg}, #task{ channelid = ChannelId} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    dgiot_bridge:send_log(ChannelId,"~s",[Payload]),
    ?LOG(info,"Payload ~s ",[Payload]),
    {noreply, State};

handle_info(connect_td, #task{key = Key} = State) ->
    case application:get_env(dgiot_td_channel, password) of
        {ok, Password} ->
            PubTopic = <<"dgtd_", Key/binary,"/connect">>,
            ?LOG(info,"PubTopic ~p",[PubTopic]),
            dgiot_mqtt:publish(?MODULE, PubTopic, dgiot_utils:to_binary(Password));
        _ -> pass
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #task{channelid = ChannelId, key = Key} = _State ) ->
    dgiot_mqtt:unsubscribe(<<"dgtd_", Key/binary,"/", ChannelId/binary>>),
    dgiot_data:delete(?DGIOT_TD_CH_WORK, {ChannelId, Key}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
