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

-module(dgiot_profile_channel).
-behavior(dgiot_channelx).
-author("jonhliu").
-include("dgiot_device.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"PROFILE">>).
-record(state, {id, mod, product, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 注册通道类型

-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    title => #{
        zh => <<"配置同步通道"/utf8>>
    },
    description => #{
        zh => <<"设备配置同步通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"vcaddr">> => #{
        order => 1,
        type => string,
        required => false,
        default => <<"all"/utf8>>,
        title => #{
            zh => <<"网关逻辑地址"/utf8>>
        },
        description => #{
            zh => <<"网关逻辑地址"/utf8>>
        }
    },
    <<"mode">> => #{
        order => 2,
        type => enum,
        required => false,
        default => #{<<"value">> => <<"manual">>, <<"label">> => <<"手动同步"/utf8>>},
        enum => [
            #{<<"value">> => <<"manual">>, <<"label">> => <<"手动同步"/utf8>>},
            #{<<"value">> => <<"auto">>, <<"label">> => <<"自动同步"/utf8>>}
        ],
        title => #{
            zh => <<"下发模式"/utf8>>
        },
        description => #{
            zh => <<"下发模式:手动同步|自动同步(设备上报配置后自动同步)"/utf8>>
        }
    },
    <<"policy">> => #{
        order => 2,
        type => enum,
        required => false,
        default => #{<<"value">> => 30, <<"label">> => <<"延迟同步"/utf8>>},
        enum => [
            #{<<"value">> => 30, <<"label">> => <<"延迟同步"/utf8>>},
            #{<<"value">> => [30, 50], <<"label">> => <<"策略同步"/utf8>>}
        ],
        title => #{
            zh => <<"同步策略"/utf8>>
        },
        description => #{
            zh => <<"同步策略:延迟同步|策略同步"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/profile.png">>,
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
    #{<<"product">> := Products} = Args,
    NewArgs = maps:without([<<"product">>], Args),
    State = #state{id = ChannelId, env = #{
        <<"products">> => Products,
        <<"args">> => NewArgs}
    },
    dgiot_data:insert({profile, channel}, ChannelId),
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"profile">>]),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

%% todo 定时自动同步，不太好判断，通过采集通道里，通过设备登录时，检查状态来进行配置同步
handle_message({sync_profile, _Pid, ProductId, DeviceAddr, DeviceProfile, Delay}, State) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    maps:fold(fun(DeviceProfileKey, UserProfileKey, Count) ->
        case maps:find(DeviceProfileKey, DeviceProfile) of
            {ok, DeviceProfileValue} ->
                BinDeviceProfileValue = dgiot_utils:to_binary(DeviceProfileValue),
                case dgiot_device:get_profile(DeviceId, UserProfileKey) of
                    not_find ->
%%                        NewDeviceProfileValue = <<" ", BinDeviceProfileValue/binary>>,
                        dgiot_device_profile:update_profile(DeviceId, #{UserProfileKey => BinDeviceProfileValue}),
                        Count;
                    BinDeviceProfileValue ->
                        Count;
                    UserProfileValue ->
                        BinUserProfileValue = dgiot_utils:to_binary(UserProfileValue),
                        case dgiot_utils:trim_string(BinUserProfileValue) of
                            BinDeviceProfileValue ->
                                Count;
                            _ ->
                                RealDelay = Delay * timer:seconds(Count),
                                erlang:send_after(RealDelay, self(), {send_profile, DeviceId, #{UserProfileKey => UserProfileValue}}),
                                Count + 1
                        end
                end;
            _ ->
                Count
        end
              end, 1, dgiot_product:get_control(ProductId)),
%%    io:format("~s ~p ~p ~p ~p ~p ~p ~n", [?FILE, ?LINE, Pid, ProductId, DeviceAddr, Profile, Delay]),
    {ok, State};

%% parse数据库里面的profile是用户想要控制设备的配置，设备的真实状态是设备上报的时候来进行比对的
handle_message({sync_parse, _Pid, 'before', put, _Token, <<"Device">>, #{<<"id">> := DeviceId} = QueryData}, State) ->
%%    io:format("~s ~p DeviceId: ~p  ~n", [?FILE, ?LINE, DeviceId]),
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
                QueryData
        end,
%%    io:format("~s ~p Template = ~ts ~n", [?FILE, ?LINE, unicode:characters_to_list(jiffy:encode(Template))]),
    dgiot_device_profile:put('before', NewQueryData),
    {ok, State};

handle_message({send_profile, DeviceId, Profile}, State) ->
    dgiot_device_profile:put('before', #{<<"id">> => DeviceId, <<"profile">> => Profile}),
    {ok, State};

handle_message(_Message, State) ->
%%    ?LOG(info, "_Message ~p", [_Message]),
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.
