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

-module(dgiot_edge_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"EDGE">>).
-define(EDGE_PRODUCTID, <<"dc85acdfec">>).
-author("johnliu").
-record(state, {id, devaddr, heartbeat, serialdelay, clients}).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).
%%-dgiot_channel(?MODULE).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"DGIOT_EDGE通道"/utf8>>
    },
    description => #{
        zh => <<"DGIOT_EDGE通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"heartbeat">> => #{
        order => 1,
        type => integer,
        required => false,
        default => 3,
        title => #{
            zh => <<"心跳/分"/utf8>>
        },
        description => #{
            zh => <<"边缘主机心跳/分"/utf8>>
        }
    },
    <<"addr">> => #{
        order => 2,
        type => string,
        required => false,
        default => <<"macaddr"/utf8>>,
        title => #{
            zh => <<"网关地址"/utf8>>
        },
        description => #{
            zh => <<"网关地址"/utf8>>
        }
    },
    <<"serialdelay">> => #{
        order => 3,
        type => integer,
        required => false,
        default => 0,
        title => #{
            zh => <<"串口延迟/毫秒"/utf8>>
        },
        description => #{
            zh => <<"串口延迟/毫秒"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/edge_channel.png">>,
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
init(?TYPE, ChannelId, ChannelArgs) ->
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"content">>]),
    Ifaddr = case maps:get(<<"addr">>, ChannelArgs, <<"macaddr">>) of
                 <<"macaddr">> ->
                     dgiot_utils:get_ifaddr("enp1s0");
                 Addr ->
                     Addr
             end,
    Heartbeat = dgiot_utils:to_int(maps:get(<<"heartbeat">>, ChannelArgs, 3)),
    Serialdelay = dgiot_utils:to_int(maps:get(<<"serialdelay">>, ChannelArgs, 0)),
    State = #state{id = ChannelId, devaddr = Ifaddr, heartbeat = Heartbeat, serialdelay = Serialdelay, clients = []},
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 10, dgiot_datetime:now_secs() + 120),
    {ok, State, dgiot_edge_tcp:childspec(ChannelId, ChannelArgs)}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message(refresh_host, #state{id = ChannelId, devaddr = Ifaddr, heartbeat = Heartbeat, clients = Clients} = State) ->
%%    Ifaddr = dgiot_utils:get_ifaddr("enp1s0"),
    Ip1 = dgiot_utils:get_ifip("enp1s0"),
    Ip2 = dgiot_utils:get_ifip("enp2s0"),
    Ip3 = dgiot_utils:get_ifip("edge0"),
    Wlp = dgiot_utils:get_ifip("wlp3s0"),
    Storage = #{
        <<"lan1">> => Ip1,
        <<"lan2">> => Ip2,
        <<"lan3">> => Ip3,
        <<"wlp">> => Wlp,
        <<"wlp_state">> => get_ipstate(Wlp),
        <<"lan1_state">> => get_ipstate(Ip1),
        <<"lan2_state">> => get_ipstate(Ip2),
        <<"lan3_state">> => get_ipstate(Ip3)
    },
%%    io:format("Storage ~p ~n",[Storage]),
    dgiot_task:save_td(?EDGE_PRODUCTID, Ifaddr, Storage, #{}),
    dgiot_edge_tcp:check_serials(ChannelId, Clients),
    erlang:send_after(Heartbeat * 60 * 1000, self(), refresh_host),
    {ok, State};

handle_message(start_client, #state{id = ChannelId, devaddr = Ifaddr, clients = OldClients, serialdelay = Serialdelay} = State) ->
%%    dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "~s ~p start_client Ifaddr ", [?FILE, ?LINE, Ifaddr]),
    NewClients =
        case dgiot_data:get({start_client, ChannelId}) of
            not_find ->
                Ip1 = dgiot_utils:get_ifip("enp1s0"),
                create_edge(Ifaddr, Ip1),
                erlang:send_after(60 * 1000, self(), refresh_host),
                dgiot_data:insert({start_client, ChannelId}, ChannelId),
                case dgiot_parse:get_object(<<"Channel">>, ChannelId) of
                    {ok, #{<<"config">> := #{<<"serials">> := Serials}}} ->
                        maps:fold(fun
                                      (ClientId, #{<<"action">> := <<"enable">>} = V, Acc) ->
                                          dgiot_edge_tcp:start(ChannelId, ClientId, V#{<<"edgeaddr">> => Ifaddr, <<"serialdelay">> => Serialdelay}),
                                          Acc ++ [ClientId];
                                      (_, _, Acc) ->
                                          Acc
                                  end, [], Serials);
                    _ ->
                        []
                end;
            _ ->
                []
        end,
    {ok, State#state{clients = lists:umerge(OldClients, NewClients)}};

handle_message({sync_parse, _Pid, 'before', put, SessionToken, <<"Device">>, #{<<"content">> := #{<<"action">> := <<"enable">>} = Content} = _Data},
    #state{id = ChannelId, devaddr = Ifaddr, clients = OldClients, serialdelay = Serialdelay} = State) ->
    NewClients =
        maps:fold(fun(Serialport, V, Acc) ->
            ClientId = <<Ifaddr/binary, "_", Serialport/binary>>,
            dgiot_mqtt:subscribe_route_key([<<"$dg/user/edge/", Serialport/binary>>], <<"dgiot_edge">>, SessionToken),
            dgiot_edge_tcp:start(ChannelId, ClientId, V#{<<"action">> => <<"enable">>, <<"serialport">> => Serialport, <<"edgeaddr">> => Ifaddr, <<"serialdelay">> => Serialdelay}),
            Acc ++ [ClientId]
                  end, [], maps:without([<<"action">>], Content)),
    {ok, State#state{clients = lists:umerge(OldClients, NewClients)}};

handle_message({sync_parse, _Pid, 'before', put, SessionToken, <<"Device">>, #{<<"id">> := _DeviceId, <<"content">> := #{<<"action">> := <<"disable">>} = Content} = _Data},
    #state{id = ChannelId, devaddr = Ifaddr} = State) ->
    maps:fold(fun(Serialport, V, _) ->
        ClientId = <<Ifaddr/binary, "_", Serialport/binary>>,
        dgiot_client:stop(ChannelId, ClientId),
        Pubtopic = <<"$dg/user/edge/", Serialport/binary>>,
        dgiot_mqtt:publish(self(), Pubtopic, <<" CLOSE SUCCESS">>),
        dgiot_mqtt:unsubscribe_mgmt(SessionToken, <<"$dg/user/edge/", Serialport/binary>>),
        case dgiot_parse:get_object(<<"Channel">>, ChannelId) of
            {ok, #{<<"config">> := Config}} ->
                Serials = maps:get(<<"serials">>, Config, #{}),
                dgiot_parse:update_object(<<"Channel">>, ChannelId, #{<<"config">> => Config#{<<"serials">> => Serials#{ClientId => V#{<<"serialport">> => Serialport, <<"action">> => <<"disable">>}}}});
            _ ->
                pass
        end,
        dgiot_task:save_td(?EDGE_PRODUCTID, Ifaddr, #{<<Serialport/binary, "_state">> => 1}, #{})
              end, #{}, maps:without([<<"action">>], Content)),
    {ok, State};

handle_message(_Message, State) ->
%%    io:format("~s ~p _Message = ~p.~n", [?FILE, ?LINE, _Message]),
%%    io:format("~s ~p State = ~p.~n", [?FILE, ?LINE, State]),
    {ok, State}.

stop(_ChannelType, ChannelId, _State) ->
    dgiot_data:delete({start_client, ChannelId}),
    ok.

create_edge(Ifaddr, Ip) ->
    case dgiot_product:lookup_prod(?EDGE_PRODUCTID) of
        {ok, #{<<"ACL">> := Acl}} ->
            dgiot_device:create_device(#{
                <<"devaddr">> => Ifaddr,
                <<"name">> => <<"区域数控一体机_"/utf8, Ifaddr/binary>>,
                <<"ip">> => Ip,
                <<"isEnable">> => true,
                <<"product">> => ?EDGE_PRODUCTID,
                <<"ACL">> => Acl,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => <<"DGIOT_EDGE">>,
                <<"devModel">> => <<"dgiot">>
            });
        _ ->
            pass
    end.


get_ipstate(Ip) ->
    case size(Ip) > 5 of
        true ->
            1;
        _ ->
            0
    end.

