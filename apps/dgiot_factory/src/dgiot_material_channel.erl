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
    dgiot_parse_hook:subscribe(<<"Device">>, post, ChannelId),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.
%%{sync_parse, self(), Type, Method, Token, Class, Data}
handle_message({sync_parse, _Pid, 'before', put, _Token, <<"Device">>, #{<<"id">> := DeviceId} = QueryData}, State) ->
    NewQueryData =
        case dgiot_device:lookup(DeviceId) of
            {ok, #{<<"productid">> := ProductId}} ->
                case catch dgiot_hook:run_hook({sync_parse, before, put, ProductId}, {QueryData, ProductId, State}) of
                    {ok, [Res]} ->
                        Res;
                    _ ->
                        QueryData
                end;
            _ ->
                case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                    {ok, #{<<"product">> := #{<<"objectId">> := ProductId}}} ->
                        case catch dgiot_hook:run_hook({sync_parse, before, put, ProductId}, {QueryData, ProductId, State}) of
                            {ok, [Res]} ->
                                Res;
                            _ ->
                                QueryData
                        end;
                    _ ->
%%                      io:format("~s ~p here  ~n", [?FILE, ?LINE]),
                        QueryData
                end
        end,
    dgiot_parse_hook:publish(_Pid, NewQueryData),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', post, _Token, <<"Device">>, #{<<"basedata">> := BaseData, <<"objectId">> := DeviceId} = QueryData}, State) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"productid">> := ProductId}} ->
            case catch dgiot_hook:run_hook({sync_parse, 'after', post, ProductId}, {QueryData, ProductId, State}) of
                {ok, [{ok,Res}]} ->
                    NewBaseData = dgiot_map:merge(BaseData,Res),
                    dgiot_parse:update_object(<<"Device">>,DeviceId,#{<<"basedata">> => NewBaseData});
                _ ->
                    QueryData
            end;
        _ ->
            pass
    end,
    {ok, State};

handle_message({_, _Pid, _, _, _Token, <<"Device">>, _QueryData} = Message, State) ->
    ?LOG(debug, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.
