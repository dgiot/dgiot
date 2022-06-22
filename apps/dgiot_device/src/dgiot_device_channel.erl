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

-module(dgiot_device_channel).
-behavior(dgiot_channelx).
-author("kenneth").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-define(TYPE, <<"DEVICE">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {id, mod, product, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).


%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    priority => 2,
    title => #{
        zh => <<"Device缓存通道"/utf8>>
    },
    description => #{
        zh => <<"Device缓存通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"order">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"createdAt"/utf8>>,
        title => #{
            zh => <<"排序"/utf8>>
        },
        description => #{
            zh => <<"排序"/utf8>>
        }
    },
    <<"offline">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 180,
        title => #{
            zh => <<"离线超时时间(秒)"/utf8>>
        },
        description => #{
            zh => <<"离线超时时间(秒)"/utf8>>
        }
    },
    <<"checktime">> => #{
        order => 3,
        type => integer,
        required => true,
        default => 3,
        title => #{
            zh => <<"设备状态落库周期(分)"/utf8>>
        },
        description => #{
            zh => <<"设备状态落库周期(分)"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/device_profile.png">>,
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
init(?TYPE, ChannelId, #{<<"offline">> := OffLine} = Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    dgiot_data:insert({device, offline}, OffLine),
    dgiot_parse_hook:subscribe(<<"Device">>, get, ChannelId),
    dgiot_parse_hook:subscribe(<<"Device">>, post, ChannelId),
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"isEnable">>]),
    dgiot_parse_hook:subscribe(<<"Device/*">>, delete, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product">>, get, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product/*">>, get, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product">>, post, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product/*">>, put, ChannelId),
    dgiot_parse_hook:subscribe(<<"Product/*">>, delete, ChannelId),
    dgiot_parse_hook:subscribe(<<"Channel/*">>, delete, ChannelId),
    {ok, State, []}.

handle_init(#state{env = #{<<"checktime">> := CheckTime}} = State) ->
    erlang:send_after(CheckTime * 60 * 1000, self(), check),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.

handle_message(check, #state{id = ChannelId, env = #{<<"offline">> := OffLine, <<"checktime">> := CheckTime}} = State) ->
    dgiot_channelx:send_after(CheckTime * 60 * 1000, ChannelId, check),
    dgiot_device:sync_parse(OffLine),
    {ok, State};


handle_message({sync_parse, Pid, 'after', get, _Token, <<"Device">>, #{<<"results">> := Results} = ResBody}, State) ->
    {NewResults, DeviceList} =
        lists:foldl(
            fun(#{<<"objectId">> := DeviceId} = Device, {NewResult, Dev}) ->
                case dgiot_device:lookup(DeviceId) of
                    {ok, #{<<"status">> := Status, <<"isEnable">> := IsEnable, <<"longitude">> := Longitude, <<"latitude">> := Latitude, <<"time">> := Time}} ->
                        NewStatus =
                            case Status of
                                true ->
                                    <<"ONLINE">>;
                                _ ->
                                    <<"OFFLINE">>
                            end,
                        Location = #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => Longitude, <<"latitude">> => Latitude},
                        {NewResult ++ [Device#{<<"location">> => Location, <<"status">> => NewStatus, <<"isEnable">> => IsEnable, <<"lastOnlineTime">> => Time}], Dev ++ [DeviceId]};
                    _ ->
                        {NewResult ++ [Device], Dev}
                end
            end, {[], []}, Results),
    SessionToken = dgiot_parse_auth:get_usersession(dgiot_utils:to_binary(_Token)),
    dgiot_mqtt:subscribe_route_key(DeviceList, SessionToken, devicestate),
    dgiot_parse_hook:publish(Pid, ResBody#{<<"results">> => NewResults}),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', post, Token, <<"Device">>, QueryData}, State) ->
    dgiot_device:post(QueryData, Token),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', put, _Token, <<"Device">>, QueryData}, State) ->
%%    io:format("~s ~p ~p  ~n", [?FILE, ?LINE, QueryData]),
    dgiot_device:put(QueryData),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', delete, _Token, <<"Device">>, ObjectId}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, ObjectId]),
    dgiot_device_hook:delete('after', ObjectId),
    {ok, State};

handle_message({sync_parse, Pid, 'after', get, Token, <<"Product">>, #{<<"results">> := _Results} = ResBody}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid,Header]),
    Key = dgiot_device_static:get_count(Token),
    timer:sleep(100),
    NewResBody = dgiot_device_static:stats(ResBody, Key),
    dgiot_parse_hook:publish(Pid, NewResBody),
    {ok, State};

handle_message({sync_parse, Pid, 'after', get, _Token, <<"Product">>, #{<<"objectId">> := _ObjectId} = ResBody}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, ObjectId]),
    dgiot_parse_hook:publish(Pid, ResBody),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', post, _Token, <<"Product">>, QueryData}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, QueryData]),
    dgiot_product_hook:post('after', QueryData),
    dgiot_product:save(QueryData),
    timer:sleep(100),
    ProductId = maps:get(<<"objectId">>, QueryData),
    dgiot_product:do_td_message(ProductId),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', put, _Token, <<"Product">>, QueryData}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, QueryData]),
    dgiot_product_hook:put('after', QueryData),
    dgiot_product:put(QueryData),
    timer:sleep(100),
    ProductId = maps:get(<<"objectId">>, QueryData),
    dgiot_product:do_td_message(ProductId),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', delete, _Token, <<"Product">>, ObjectId}, State) ->
%%    io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, Pid, ObjectId]),
    dgiot_product_hook:delete('after', ObjectId),
    dgiot_product:delete(ObjectId),
    {ok, State};

handle_message({sync_parse, _Pid, 'before', delete, _Token, <<"Channel">>, ObjectId}, State) ->
%%    io:format("~s ~p ~p ~n", [?FILE, ?LINE, ObjectId]),
    case dgiot_parse:get_object(<<"Channel">>, ObjectId) of
        {ok, #{<<"isEnable">> := true}} ->
            dgiot_bridge:control_channel(ObjectId, <<"disable">>);
        _ -> pass
    end,
    {ok, State};

handle_message({update_schemas_json}, State) ->
%%    更新表字段
    dgiot_parse:update_schemas_json(),
    {ok, State};

handle_message(Message, State) ->
    ?LOG(debug, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.

