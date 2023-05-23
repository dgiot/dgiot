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

-module(dgiot_factory_channel).
-behavior(dgiot_channelx).
-author("kenneth").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_factory.hrl").
-define(TYPE, <<"FACTORY">>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(MAX_BUFF_SIZE, 1024).

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
        zh => <<"数字工厂通道"/utf8>>
    },
    description => #{
        zh => <<"数字工厂通道"/utf8>>
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
init(?TYPE, ChannelId, #{<<"product">> := Products} = Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    lists:map(
        fun
            ({ProductId, _}) ->
                case dgiot_product:get(ProductId) of
                    {ok, #{<<"devType">> := DevType, <<"name">> := Name}} ->
                        TempProductId =dgiot_factory_worker:get_wokrer_id(Name, DevType),
                        case TempProductId of
                            ProductId ->
                                dgiot_data:insert({ChannelId, worker}, ProductId);
                            _ ->
                                pass
                        end;
                    _ ->
                        pass
                end
        end, Products),
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"content">>]),
    dgiot_parse_hook:subscribe(<<"Device/*">>, delete, ChannelId),
    dgiot_parse_hook:subscribe(<<"_User/*">>, post, ChannelId),
    dgiot_parse_hook:subscribe(<<"_User/*">>, put, ChannelId),
    dgiot_parse_hook:subscribe(<<"_User/*">>, delete, ChannelId),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.


handle_message({sync_parse, _Pid, 'after', put, _Token, <<"_User">>, #{<<"objectId">> := UserId} = _QueryData},
    #state{id = ChannelId} = State) ->
    case dgiot_data:get({ChannelId, worker}) of
        not_find ->
            pass;
        ProductId ->
            case dgiot_parse:get_object(<<"_User">>, UserId) of
                {ok, #{<<"username">> := WorkerNum, <<"nick">> := WorkerName}} ->
                    dgiot_factory_worker:init_worker_device(ProductId, WorkerNum, WorkerName);
                _ ->
                    pass
            end
    end,
    {ok, State};
handle_message({sync_parse, _Pid, 'after', post, _Token, <<"_User">>, #{<<"objectId">> := UserId} = _QueryData},
    #state{id = ChannelId} = State) ->
    case dgiot_data:get({ChannelId, worker}) of
        not_find ->
            pass;
        ProductId ->
            case dgiot_parse:get_object(<<"_User">>, UserId) of
                {ok, #{<<"username">> := WorkerNum, <<"nick">> := WorkerName}} ->
                    dgiot_factory_worker:init_worker_device(ProductId, WorkerNum, WorkerName);
                _ ->
                    pass
            end
    end,
    {ok, State};

handle_message({sync_parse, _Pid, 'before', put, Token, <<"Device">>,
    #{<<"content">> := #{<<"person">> := #{<<"type">> := PersonType}} = Content, <<"id">> := TaskDeviceId} = _QueryData},
    #state{id = ChannelId} = State) ->
    io:format("~s  ~p  TaskDeviceId= ~p ~n", [?FILE, ?LINE, TaskDeviceId]),
    case dgiot_factory_utils:get_productId(TaskDeviceId) of
        {ok,  TaskProductId} ->
            dgiot_metrics:inc(dgiot_factory, <<"input_num">>, 1),
            case dgiot_factory_batch:merge_data(Content, PersonType, Token, TaskDeviceId) of
                {BatchProductId, BatchDeviceId, BatchAddr, NewData} ->
                    dgiot_factory_form:do_form(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, BatchAddr, PersonType, NewData, ChannelId),
                    {ok, State};
                _ ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end;

handle_message(Message, State) ->
    ?LOG(debug, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.
