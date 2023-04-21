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
-module(dgiot_notification_channel).
-behavior(dgiot_channelx).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"NOTIFICATION">>).
-record(state, {id, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3, test/1]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    title => #{
        zh => <<"NOTIFICATION通道"/utf8>>
    },
    description => #{
        zh => <<"NOTIFICATION通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"msgtype">> => #{
        order => 2,
        type => enum,
        required => false,
        default => <<"小程序"/utf8>>,
        enum => [
            #{<<"value">> => <<"wechat">>, <<"label">> => <<"小程序"/utf8>>},
            #{<<"value">> => <<"email">>, <<"label">> => <<"邮件"/utf8>>},
            #{<<"value">> => <<"sms">>, <<"label">> => <<"短信"/utf8>>},
            #{<<"value">> => <<"umeng">>, <<"label">> => <<"友盟通知"/utf8>>}
        ],
        title => #{
            zh => <<"通知方式"/utf8>>
        },
        description => #{
            zh => <<"通知方式:wechat|email|sms|umeng"/utf8>>
        }
    },
    <<"defultname">> => #{
        order => 3,
        type => string,
        required => false,
        default => <<"dgiot_notification">>,
        title => #{
            zh => <<"默认通道名"/utf8>>
        },
        description => #{
            zh => <<"默认通道名,有则覆盖手动填入的通道名称"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/NOTIFICATION.png">>,
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
init(?TYPE, ChannelId, Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    dgiot_notification:save_configuration(),
    dgiot_parse_hook:subscribe(<<"Notification">>, get, ChannelId),
    dgiot_parse_hook:subscribe(<<"Notification/*">>, put, ChannelId, [<<"status">>]),
    dgiot_parse_hook:subscribe(<<"Maintenance">>, post, ChannelId),
%%    dgiot_parse_id:get_channelid(?BACKEND_CHL, ?TYPE, <<"dgiot_notification">>),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

% SELECT clientid, payload, topic FROM "meter"
% SELECT clientid, disconnected_at FROM "$events/client_disconnected" WHERE username = 'dgiot'
% SELECT clientid, connected_at FROM "$events/client_connected" WHERE username = 'dgiot'
handle_message({rule, #{clientid := _DevAddr, connected_at := _ConnectedAt} = _Msg, _Context}, State) ->
    {ok, State};

handle_message({rule, #{clientid := _DevAddr, disconnected_at := _DisconnectedAt} = _Msg, _Context}, State) ->
    {ok, State};

%% SELECT payload.electricity as electricity FROM  "$dg/user/alarm/94656917ab/157d0ff60f/#" where electricity  >  20
handle_message({rule, #{metadata := #{rule_id := <<"rule:Notification_", Ruleid/binary>>}, clientid := DeviceId, payload := _Payload, topic := _Topic} = _Msg, Context}, State) ->
    NewContext =
        maps:fold(fun(Key, Value, Acc) ->
            Acc#{<<"dgiot_alarmkey">> => Key, <<"dgiot_alarmvalue">> => Value}
                  end, #{}, Context),
    dgiot_umeng:add_notification(Ruleid, DeviceId, NewContext),
    {ok, State};

handle_message({sync_parse, Pid, 'after', get, _Token, <<"Notification">>, #{<<"results">> := _Results} = ResBody}, State) ->
    timer:sleep(100),
    NewResBody = dgiot_notification:get_newbody(ResBody),
    dgiot_parse_hook:publish(Pid, NewResBody),
    {ok, State};

handle_message({sync_parse, _Pid, 'after', put, _Token, <<"Notification">>, #{<<"objectId">> := ObjectId} = QueryData}, State) ->
%%    io:format("~s ~p ~p  ~n", [?FILE, ?LINE, QueryData]),
    case maps:find(<<"status">>, QueryData) of
        {ok, 2} ->
%%            告警手动恢复
            dgiot_umeng:manual_recovery(ObjectId);
        _ ->
            pass
    end,
    {ok, State};

handle_message({sync_parse, _Pid, 'after', post, _Token, <<"Maintenance">>, QueryData}, State) ->
    dgiot_maintenance:init_inspection(QueryData),
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.


test(ProductId) ->
    spawn(fun() ->
        timer:sleep(3 * 100),
        lists:foreach(
            fun(I) ->
                DeviceId = dgiot_parse_id:get_deviceid(ProductId, dgiot_utils:to_binary(I)),
                Body = #{
                    <<"ACL">> => #{
                        <<"*">> => #{
                            <<"read">> => true,
                            <<"write">> => true
                        }
                    },
                    <<"content">> => #{<<"alertstatus">> => true, <<"_deviceid">> => DeviceId, <<"_productid">> => ProductId},
                    <<"public">> => true,
                    <<"status">> => 0,
                    <<"sender">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"_User">>,
                        <<"objectId">> => <<"Klht7ERlYn">>
                    },
                    <<"type">> => <<ProductId/binary, "_alarm">>
                },
                dgiot_parse:create_object(<<"Notification">>, Body)
            end, lists:seq(1, 6000))
          end).
