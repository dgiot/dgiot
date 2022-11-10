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

-module(dgiot_material_channel).
-behavior(dgiot_channelx).
-author("kenneth").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_factory.hrl").
-define(TYPE, <<"MATERIAL">>).
-record(state, {id, mod, product, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
-export([handle_material_apply/2]).
-export([handle_apply_form/2]).
%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    priority => 2,
    title => #{
        zh => <<"物料申请通道"/utf8>>
    },
    description => #{
        zh => <<"物料申请通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/factory.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).


start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{<<"product">> := _Products} = Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"basedata">>]),
    dgiot_parse_hook:subscribe(<<"Device/*">>, post, ChannelId),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.
%%{sync_parse, self(), Type, Method, Token, Class, Data}
handle_message({sync_parse, _Pid, 'before', put, _Token, <<"Device">>, #{<<"id">> := DeviceId} = QueryData}, State) ->
    io:format("~s ~p DeviceId = ~p ~n", [?FILE, ?LINE, DeviceId]),
    NewQueryData =
        case dgiot_device:lookup(DeviceId) of
            {ok, #{<<"productid">> := ProductId}} ->
                case catch dgiot_hook:run_hook({sync_parse, before, put, ProductId}, {QueryData, ProductId, State}) of
                    {ok, [Res]} ->
%%                        io:format("~s ~p Res = ~ts.~n", [?FILE, ?LINE, unicode:characters_to_list(jsx:encode(Res))]),
                        Res;
                    _ ->
                        QueryData
                end;
            _->
                pass
        end,
    dgiot_parse_hook:publish(_Pid, NewQueryData),
    {ok, State};


%%handle_message({sync_parse, _Pid, 'before', put, _Token, <<"Device">>, #{<<"basedata">> := _BaseData,
%%    <<"objectId">> := DeviceId}},
%%    State) ->
%%    io:format("~s ~p DeviceId = ~p ~n", [?FILE, ?LINE, DeviceId]),
%%    handle_apply_form(#{a => a}, DeviceId),
%%    {ok, State};


%%handle_message({sync_parse, _Pid, 'after', put, _Token, <<"Device">>, _QueryData},
%%    State) ->
%%%%    io:format("~s ~p Message = ~p ~n", [?FILE, ?LINE, Message]),
%%    {ok, State};
handle_message({_, _Pid, _, _, _Token, <<"Device">>, _QueryData} = Message, State) ->
%%    io:format("~s ~p Message = ~p ~n", [?FILE, ?LINE, Message]),
%%    io:format("~s  ~p  QueryData= ~ts ~n", [?FILE, ?LINE, unicode:characters_to_list(jiffy:encode(_QueryData))]),
    ?LOG(debug, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.

handle_apply_form(#{<<"Status">> := 21, <<"SumPick">> := -1, <<"PickList">> := BatchList} = BaseData, DeviceId) when is_list(BatchList) ->
    {NewList, Sum} = lists:foldl(
        fun
            (#{<<"PickNum">> := PickNum, <<"objectId">> := MaterialId} = Batch, {List, Sum}) ->
                case dgiot_parse:get_object(<<"Device">>, MaterialId) of
                    {ok, #{<<"content">> := #{<<"FQty">> := FQty} = MaterialContent}} ->
                        UnConfirm = maps:get(<<"UnConform">>, MaterialContent, 0),
                        NewUnConfirm = UnConfirm + PickNum,
                        Remaining = maps:get(<<"Remaining">>, MaterialContent, FQty),
                        NewRemaining = Remaining - dgiot_utils:to_int(PickNum),
                        NewContent = MaterialContent#{<<"Remaining">> => NewRemaining, <<"unConform">> => NewUnConfirm},
                        dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"content">> => NewContent}),
                        {List ++ [Batch], Sum + PickNum};
                    _ ->
                        io:format("~s ~p MaterialId = ~p ~n", [?FILE, ?LINE, MaterialId]),
                        {List, Sum}
                end;
            (_, Acc) ->
                Acc
        end, {[], 0}, BatchList),
    NewBaseData = dgiot_map:merge(BaseData, #{<<"BatchList">> => NewList, <<"SumPick">> => Sum}),
    io:format("~s ~p SumPick = ~p ~n", [?FILE, ?LINE, Sum]),
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => NewBaseData});

%%handle_apply_form(#{<<"Status">> := 0, <<"PickList">> := BatchList} = BaseData, DeviceId) ->
%%    lists:foldl(
%%        fun
%%            (#{<<"PickNum">> := PickNum, <<"objectId">> := MaterialId} = Batch, {List, Sum}) ->
%%                case dgiot_parse:get_object(<<"Device">>, MaterialId) of
%%                    {ok, #{<<"content">> := #{<<"FQty">> := FQty} = MaterialContent}} ->
%%                        UnConfirm = maps:get(<<"UnConform">>, MaterialContent, 0),
%%                        NewUnConfirm = UnConfirm - PickNum,
%%                        Remaining = maps:get(<<"Remaining">>, MaterialContent, FQty),
%%                        NewRemaining = Remaining - dgiot_utils:to_int(PickNum),
%%                        NewContent = MaterialContent#{<<"Remaining">> => NewRemaining, <<"unConform">> => NewUnConfirm},
%%                        dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"content">> => NewContent}),
%%                        {List ++ [Batch], Sum + PickNum};
%%                    _ ->
%%                        io:format("~s ~p MaterialId = ~p ~n", [?FILE, ?LINE, MaterialId]),
%%                        {List, Sum}
%%                end;
%%            (_, Acc) ->
%%                Acc
%%        end, {[], 0}, BatchList);

handle_apply_form(_, _) ->
    pass.

handle_material_apply(DeviceId, #{<<"Status">> := <<"21">>, <<"PickList">> := BatchList} = BaseData) when is_list(BatchList) ->
    {NewList, Sum} = lists:foldl(
        fun
            (#{<<"PickNum">> := PickNum, <<"objectId">> := MaterialId} = Batch, {List, Sum}) ->
                case dgiot_parse:get_object(<<"Device">>, MaterialId) of
                    {ok, #{<<"content">> := #{<<"FQty">> := FQty} = MaterialContent}} ->
                        io:format("~s ~p PickNum = ~p ~n", [?FILE, ?LINE, PickNum]),
                        UnConfirm = maps:get(<<"UnConform">>, MaterialContent, 0),
                        NewUnConfirm = UnConfirm + PickNum,
                        Remaining = maps:get(<<"Remaining">>, MaterialContent, FQty),
                        NewRemaining = Remaining - dgiot_utils:to_int(PickNum),
                        NewContent = MaterialContent#{<<"Remaining">> => NewRemaining, <<"unConform">> => NewUnConfirm},
                        dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"content">> => NewContent}),
                        {List ++ [Batch], Sum + PickNum};
                    _ ->
                        io:format("~s ~p MaterialId = ~p ~n", [?FILE, ?LINE, MaterialId]),
                        {List, Sum}
                end;
            (_, Acc) ->
                Acc
        end, {[], 0}, BatchList),
    NewBaseData = dgiot_map:merge(BaseData, #{<<"PickList">> => NewList, <<"SumPick">> => Sum}),
    io:format("~s ~p SumPick = ~p ~n", [?FILE, ?LINE, Sum]),
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => NewBaseData});


handle_material_apply(DeviceId, #{<<"Status">> := <<"11">>, <<"PickList">> := BatchList} = BaseData) ->
    lists:foldl(
        fun
            (#{<<"PickNum">> := PickNum, <<"objectId">> := MaterialId}, _) ->
                case dgiot_parse:get_object(<<"Device">>, MaterialId) of
                    {ok, #{<<"content">> := #{<<"FQty">> := FQty} = MaterialContent}} ->
                        UnConfirm = maps:get(<<"UnConform">>, MaterialContent, 0),
                        NewUnConfirm = UnConfirm - PickNum,
                        Remaining = maps:get(<<"Remaining">>, MaterialContent, FQty),
                        NewRemaining = Remaining + dgiot_utils:to_int(PickNum),
                        NewContent = MaterialContent#{<<"Remaining">> => NewRemaining, <<"unConform">> => NewUnConfirm},
                        dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"content">> => NewContent});
                    _ ->
                        pass
                end;
            (_, _) ->
                pass
        end, [], BatchList),
    NewBaseData = dgiot_map:merge(BaseData, #{<<"BatchList">> => [], <<"SumPick">> => -1}),
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => NewBaseData});

handle_material_apply(DeviceId, #{<<"Status">> := 0, <<"PickList">> := BatchList, <<"SumPick">> := _SumPick} = BaseData) ->
    lists:foldl(
        fun
            (#{<<"PickNum">> := PickNum, <<"objectId">> := MaterialId}, _) ->
                case dgiot_parse:get_object(<<"Device">>, MaterialId) of
                    {ok, #{<<"content">> := MaterialContent}} ->
                        UnConfirm = maps:get(<<"UnConform">>, MaterialContent, 0),
                        NewUnConfirm = UnConfirm - PickNum,
                        NewContent = MaterialContent#{<<"unConform">> => NewUnConfirm},
                        dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"content">> => NewContent});
                    _ ->
                        pass
                end;
            (_, _) ->
                pass
        end, [], BatchList),
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"basedata">> => BaseData}),
    dgiot_hook:run_hook();
handle_material_apply(_, BaseData) ->
    io:format("~s ~p BaseData = ~p  ~n", [?FILE, ?LINE, BaseData]),
    pass.
