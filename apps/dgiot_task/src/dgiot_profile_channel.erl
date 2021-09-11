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
-include("dgiot_task.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"PROFILE">>).
-record(state, {id, mod, product, env = #{}}).

-dgiot_data("ets").
-export([init_ets/0]).
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
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/%E6%8C%87%E4%BB%A4%E4%BB%BB%E5%8A%A1%E5%9B%BE%E6%A0%87.png">>,
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

init_ets() ->
    dgiot_data:init(?PROFILE),
    dgiot_data:init(?MODIFYPROFILE).

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
    {ok, State, []}.

handle_init(#state{id = _ChannelId, env = #{<<"products">> := _Products, <<"args">> := _Args}} = State) ->
    erlang:send_after(1000, self(), {message, <<"_Pool">>, check_profile}),
    dgiot_parse:subscribe(<<"Device/*">>, put),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message({check_profie, Args}, State) ->
    ?LOG(info, "Args ~p", [Args]),
    Fun = fun(X) ->
        ?LOG(info, "X ~p", [X])
          end,
    dgiot_data:loop(?MODIFYPROFILE, Fun),
    erlang:send_after(1000 * 30, self(), {message, <<"_Pool">>, check_profile}),
    {ok, State};

handle_message({sync_parse, Args}, State) ->
%%    ?LOG(info, "Args ~p", [jsx:decode(Args, [{labels, binary}, return_maps])]),
    case jsx:decode(Args, [{labels, binary}, return_maps]) of
        #{<<"profile">> := Profile, <<"devaddr">> := Devaddr, <<"product">> := #{<<"objectId">> := ProductId}} = Arg ->
            Sessiontoken = maps:get(<<"sessiontoken">>, Arg, <<"">>),
            DeviceId = dgiot_parse:get_deviceid(ProductId, Devaddr),
            Modifyprofile = get_modifyprofile(DeviceId, Profile),
%%            设置参数
            case dgiot_device:get_online(DeviceId) of
                true ->
                    case dgiot_parse:get_object(<<"Product">>, ProductId) of
                        {ok, #{<<"config">> := #{<<"basedate">> := #{<<"params">> := Params}}}} ->
                            lists:foldl(fun(X, _Acc) ->
                                case X of
                                    #{<<"identifier">> := Identifier, <<"protocol">> := Proctol} ->
                                        case maps:find(Identifier, Modifyprofile) of
                                            {ok, V} ->
                                                Topic = <<"profile/", ProductId/binary, "/", Devaddr/binary>>,
                                                Payload = #{
                                                    <<"_dgiotprotocol">> => Proctol,
                                                    <<"sessiontoken">> => Sessiontoken,
                                                    Identifier => V
                                                },
                                                dgiot_mqtt:publish(DeviceId, Topic, Payload),
                                                timer:sleep(1000);
                                            _ ->
                                                pass
                                        end
                                end
                                        end, [], Params),
                            dgiot_data:insert(?PROFILE, DeviceId, Profile);
                        false ->
                            dgiot_data:insert(?MODIFYPROFILE, DeviceId, {Modifyprofile, ProductId, Devaddr})
                    end;
                _ ->
                    pass
            end;
        _Other ->
            pass
    end,
    {ok, State};

handle_message(_Message, State) ->
%%    ?LOG(info, "_Message ~p", [_Message]),
    {ok, State}.

stop(ChannelType, ChannelId, #state{env = #{<<"product">> := ProductId, <<"args">> := Args}} = _State) ->
    spawn(fun() ->
        dgiot_task:stop(Args#{<<"product">> => ProductId, <<"channel">> => ChannelId})
          end),
    ?LOG(warning, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.


get_modifyprofile(DeviceId, Profile) ->
    case dgiot_data:get(dgiot_profile, <<"b3de3bd7a9">>) of
        not_find ->
            dgiot_data:insert(?PROFILE, DeviceId, Profile),
            Profile;
        OldProfile ->
            maps:fold(fun(K, V, Acc) ->
                case maps:find(K, OldProfile) of
                    error ->
                        Acc#{K => V};
                    {ok, V} ->
                        Acc;
                    _ ->
                        Acc#{K => V}
                end
                      end, #{}, Profile)

    end.
