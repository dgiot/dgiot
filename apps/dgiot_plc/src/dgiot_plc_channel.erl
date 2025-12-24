%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_plc_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"DGIOTPLC">>).
-author("johnliu").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-record(state, {id, decoder = s7_decoder, step, env = #{}}).
-dgiot_channel(?MODULE).

-dgiot_data("ets").
-export([init_ets/0]).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{

    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    title => #{
        zh => <<"PLC采集通道"/utf8>>
    },
    description => #{
        zh => <<"PLC采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"mode">> => #{
        order => 1,
        type => enum,
        required => true,
        default => #{<<"value">> => <<"s7">>, <<"label">> => <<"SIEMENS S7"/utf8>>},
        enum => [
            #{<<"value">> => <<"s7">>, <<"label">> => <<"SIEMENS S7"/utf8>>}
        ],
        title => #{
            zh => <<"PLC协议类型"/utf8>>
        },
        description => #{
            zh => <<"PLC协议类型"/utf8>>
        }
    },
    <<"s7_type">> => #{
        order => 2,
        type => enum,
        required => true,
        default => #{<<"value">> => <<"s71200">>, <<"label">> => <<"s71200"/utf8>>},
        enum => [
            #{<<"value">> => <<"s7200">>, <<"label">> => <<"S7200"/utf8>>},
            #{<<"value">> => <<"s7200Smart">>, <<"label">> => <<"S7200Smart"/utf8>>},
            #{<<"value">> => <<"s7300">>, <<"label">> => <<"S7300"/utf8>>},
            #{<<"value">> => <<"s7400">>, <<"label">> => <<"S7400"/utf8>>},
            #{<<"value">> => <<"s71200">>, <<"label">> => <<"S71200"/utf8>>},
            #{<<"value">> => <<"s71500">>, <<"label">> => <<"S71500"/utf8>>}
        ],
        title => #{
            zh => <<"S7类型"/utf8>>
        },
        description => #{
            zh => <<"S7类型"/utf8>>
        }
    },
    <<"ip">> => #{
        key => <<"ip">>,
        order => 3,
        type => string,
        required => true,
        default => <<"127.0.0.1"/utf8>>,
        title => #{
            zh => <<"地址"/utf8>>
        },
        description => #{
            zh => <<"PLC服务器地址"/utf8>>
        }
    },
    <<"port">> => #{
        key => <<"port">>,
        order => 4,
        type => integer,
        required => true,
        default => 102,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"服务器端口"/utf8>>
        }
    },
    <<"freq">> => #{
        order => 5,
        type => integer,
        required => true,
        default => 10,
        title => #{
            zh => <<"采集频率/秒"/utf8>>
        },
        description => #{
            zh => <<"采集频率/秒"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/plc_channel.png">>,
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
    dgiot_data:init(dgiot_dbque).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => 1
    }).

%% 通道初始化
init(?TYPE, ChannelId, #{
    <<"Size">> := Size,
    <<"mode">> := Mode,
    <<"s7_type">> := S7_type,
    <<"ip">> := Ip,
    <<"port">> := Port} = Args) ->
    Decoder = dgiot_utils:to_atom(<<Mode/binary, "_decoder">>),
    NewArgs = #{
        <<"ip">> => Ip,
        <<"port">> => Port,
        <<"mod">> => dgiot_plc_tcp,
        <<"child">> => #{}
    },
    State = #state{
        id = ChannelId,
        env = #{
            size => Size,
            decoder => Decoder,
            dtuType => Mode,
            s7_type => S7_type,
            freq => maps:get(<<"freq">>, Args, 10)
        }
    },
    dgiot_parse_hook:subscribe(<<"Product/*">>, put, ChannelId, [<<"profile">>]),
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 5000, dgiot_datetime:now_secs() + 300000),
    {ok, State, dgiot_client:register(ChannelId, tcp_client_sup, NewArgs)}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventId, Event]),
    ok.

handle_message(check_connection, #state{id = ChannelId, env = #{plc_list := Plc_list}} = Dclient) ->
    lists:foldl(fun
                    (#{<<"objectId">> := GroupId, <<"profile">> := #{<<"ip">> := Ip, <<"mode">> := Mode, <<"s7_type">> := S7_type, <<"freq">> := Freq, <<"port">> := Port}}, _) ->
                        Now = dgiot_datetime:now_secs(),
                        case dgiot_data:get({check_connection, ChannelId, GroupId}) of
                            OldTime when (Now - OldTime) > Freq ->
                                Decoder = dgiot_utils:to_atom(<<Mode/binary, "_decoder">>),
                                Args = #{
                                    <<"ip">> => Ip,
                                    <<"port">> => Port,
                                    <<"mod">> => dgiot_plc_tcp,
                                    <<"child">> => #{
                                        <<"ip">> => Ip,
                                        <<"decoder">> => Decoder,
                                        <<"product">> => GroupId,
                                        <<"dtuType">> => Mode,
                                        <<"s7_type">> => S7_type,
                                        <<"freq">> => Freq
                                    }},
                                dgiot_client:stop(ChannelId, GroupId),
                                dgiot_client:start(ChannelId, GroupId, Args);
                            _ ->
                                pass
                        end;
                    (_, _) ->
                        pass
                end, {}, Plc_list),
    erlang:send_after(20 * 1000, self(), check_connection),
    {noreply, Dclient};

handle_message(start_client, #state{id = ChannelId, env = #{size := _Size}} = State) ->
    Plc_list =
        case dgiot_data:get({start_client, ChannelId}) of
            not_find ->
                case dgiot_parsex:query_object(<<"Product">>, #{<<"count">> => <<"objectId">>, <<"keys">> => [<<"profile">>, <<"name">>], <<"where">> => #{<<"channel.objectId">> => ChannelId}}) of
                    {ok, #{<<"count">> := Count, <<"results">> := Groups}} when Count > 0 ->
                        dgiot_data:insert({start_client, ChannelId}, ChannelId),
                        lists:foldl(fun
                                        (#{<<"objectId">> := GroupId, <<"name">> := GroupName,
                                            <<"profile">> := #{<<"ip">> := GroupIp, <<"mode">> := GroupMode, <<"s7_type">> := GroupS7_type, <<"freq">> := GroupFreq, <<"port">> := GroupPort}} = Group, Acc) ->
                                            case dgiot_parsex:query_object(<<"Product">>, #{<<"count">> => <<"objectId">>, <<"keys">> => [<<"profile">>, <<"name">>],
                                                <<"where">> => #{<<"$relatedTo">> => #{<<"object">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Product">>, <<"objectId">> => GroupId}, <<"key">> => <<"children">>}}}) of
                                                {ok, #{<<"count">> := PCount, <<"results">> := Products}} when PCount > 0 ->
                                                    lists:foldl(fun
                                                                    (#{<<"objectId">> := ProductId, <<"name">> := ProductName,
                                                                        <<"profile">> := #{<<"ip">> := Ip, <<"mode">> := Mode, <<"s7_type">> := S7_type, <<"freq">> := Freq, <<"port">> := Port}} = Product, Acc1) ->
                                                                        Decoder = dgiot_utils:to_atom(<<Mode/binary, "_decoder">>),
                                                                        Args = #{
                                                                            <<"ip">> => Ip,
                                                                            <<"port">> => Port,
                                                                            <<"mod">> => dgiot_plc_tcp,
                                                                            <<"child">> => #{
                                                                                <<"ip">> => Ip,
                                                                                <<"decoder">> => Decoder,
                                                                                <<"product">> => ProductId,
                                                                                <<"dtuType">> => Mode,
                                                                                <<"s7_type">> => S7_type,
                                                                                <<"freq">> => Freq
                                                                            }},
                                                                        dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "start_client Product enable ~p ~ts~n", [ProductId, unicode:characters_to_list(ProductName)]),
                                                                        dgiot_client:start(ChannelId, ProductId, Args),
                                                                        Acc1 ++ [Product];
                                                                    (_, Acc1) ->
                                                                        Acc1
                                                                end, Acc, Products);
                                                _ ->
                                                    Decoder = dgiot_utils:to_atom(<<GroupMode/binary, "_decoder">>),
                                                    Args = #{
                                                        <<"ip">> => GroupIp,
                                                        <<"port">> => GroupPort,
                                                        <<"mod">> => dgiot_plc_tcp,
                                                        <<"child">> => #{
                                                            <<"ip">> => GroupIp,
                                                            <<"decoder">> => Decoder,
                                                            <<"product">> => GroupId,
                                                            <<"dtuType">> => GroupMode,
                                                            <<"s7_type">> => GroupS7_type,
                                                            <<"freq">> => GroupFreq
                                                        }},
                                                    dgiot_client:start(ChannelId, GroupId, Args),
                                                    dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "sync_parse Group enable ~p ~ts~n", [GroupId, unicode:characters_to_list(GroupName)]),
                                                    Acc ++ [Group]
                                            end;
                                        (_, Acc) ->
                                            Acc
                                    end, [], Groups);
                    _ ->
                        []
                end;
            _ ->
                []
        end,
    erlang:send_after(1000, self(), check_connection),
    {ok, State#state{env = #{plc_list => Plc_list}}};

handle_message({sync_parse, _Pid, 'before', put, _Token, <<"Product">>, #{<<"id">> := GroupId, <<"name">> := GroupName,
    <<"profile">> := #{<<"action">> := <<"enable">>, <<"ip">> := Ip, <<"mode">> := Mode, <<"s7_type">> := S7_type, <<"freq">> := Freq, <<"port">> := Port}}},
    #state{id = ChannelId} = State) ->
    case dgiot_parsex:query_object(<<"Product">>, #{<<"count">> => <<"objectId">>, <<"keys">> => [<<"profile">>, <<"name">>],
        <<"where">> => #{<<"$relatedTo">> => #{<<"object">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Product">>, <<"objectId">> => GroupId}, <<"key">> => <<"children">>}}}) of
        {ok, #{<<"count">> := Count, <<"results">> := Products}} when Count > 0 ->
            lists:foldl(fun
                            (#{<<"objectId">> := ProductId, <<"name">> := ProductName,
                                <<"profile">> := #{<<"ip">> := Ip1, <<"mode">> := Mode1, <<"s7_type">> := S7_type1, <<"freq">> := Freq1, <<"port">> := Port1}} = Product, Acc1) ->
                                Decoder1 = dgiot_utils:to_atom(<<Mode1/binary, "_decoder">>),
                                Args = #{
                                    <<"ip">> => Ip1,
                                    <<"port">> => Port1,
                                    <<"mod">> => dgiot_plc_tcp,
                                    <<"child">> => #{
                                        <<"ip">> => Ip1,
                                        <<"decoder">> => Decoder1,
                                        <<"product">> => ProductId,
                                        <<"dtuType">> => Mode1,
                                        <<"s7_type">> => S7_type1,
                                        <<"freq">> => Freq1
                                    }},
                                dgiot_client:start(ChannelId, ProductId, Args),
                                dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "sync_parse Product enable ~p ~ts~n", [ProductId, unicode:characters_to_list(ProductName)]),
                                Acc1 ++ [Product];
                            (_, Acc1) ->
                                Acc1
                        end, [], Products);
        _ ->
            Decoder = dgiot_utils:to_atom(<<Mode/binary, "_decoder">>),
            Args = #{
                <<"ip">> => Ip,
                <<"port">> => Port,
                <<"mod">> => dgiot_plc_tcp,
                <<"child">> => #{
                    <<"ip">> => Ip,
                    <<"decoder">> => Decoder,
                    <<"product">> => GroupId,
                    <<"dtuType">> => Mode,
                    <<"s7_type">> => S7_type,
                    <<"freq">> => Freq
                }},
            dgiot_client:start(ChannelId, GroupId, Args),
            dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "sync_parse Group enable ~p ~ts~n", [GroupId, unicode:characters_to_list(GroupName)])
    end,
    {ok, State};

handle_message({sync_parse, _Pid, 'before', put, _Token, <<"Product">>, #{<<"id">> := GroupId, <<"name">> := GroupName, <<"profile">> := #{<<"action">> := <<"disable">>}}},
    #state{id = ChannelId} = State) ->
    case dgiot_parsex:query_object(<<"Product">>, #{<<"count">> => <<"objectId">>, <<"keys">> => [<<"profile">>, <<"name">>],
        <<"where">> => #{<<"$relatedTo">> => #{<<"object">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Product">>, <<"objectId">> => GroupId}, <<"key">> => <<"children">>}}}) of
        {ok, #{<<"count">> := Count, <<"results">> := Products}} when Count > 0 ->
            lists:foldl(fun
                            (#{<<"objectId">> := ProductId, <<"name">> := ProductName}, _) ->
                                dgiot_client:stop(ChannelId, ProductId),
                                dgiot_data:delete({check_connection, ChannelId, ProductId}),
                                dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "sync_parse Product disable ~p ~ts~n", [ProductId, unicode:characters_to_list(ProductName)]);
                            (_, _) ->
                                pass
                        end, [], Products);
        _ ->
            dgiot_client:stop(ChannelId, GroupId),
            dgiot_data:delete({check_connection, ChannelId, GroupId}),
            dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), "sync_parse Group disable ~p ~ts~n", [GroupId, unicode:characters_to_list(GroupName)])
    end,
    {ok, State};

handle_message(_Message, State) ->
%%    io:format("~s ~p Message =~p.~n", [?FILE, ?LINE, _Message]),
%%    io:format("~s ~p State =~p.~n", [?FILE, ?LINE, State]),
    {ok, State}.

stop(_ChannelType, ChannelId, _State) ->
    dgiot_parse_hook:unsubscribe(<<"Product/*">>, put, ChannelId, [<<"profile">>]),
    dgiot_data:delete({check_connection, ChannelId}),
    dgiot_data:delete({start_client, ChannelId}),
    dgiot_client:unregister(ChannelId),
    ok.

%%create_product(ChannelId, ProductName, S7_type) ->
%%    TdChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"TD">>, <<"TD资源通道"/utf8>>),
%%    Acl = dgiot_plc:get_channelAcl(ChannelId),
%%    ProductBody = #{
%%        <<"name">> => ProductName,
%%        <<"devType">> => S7_type,
%%        <<"category">> => #{<<"objectId">> => <<"2442284cf5">>, <<"__type">> => <<"Pointer">>, <<"className">> => <<"Category">>},
%%        <<"desc">> => S7_type,
%%        <<"channel">> => #{<<"type">> => 1, <<"tdchannel">> => TdChannelId, <<"otherchannel">> => [ChannelId]},
%%        <<"ACL">> => Acl,
%%        <<"nodeType">> => 0,
%%        <<"thing">> => #{},
%%        <<"productSecret">> => dgiot_utils:random()
%%    },
%%    ProductId = dgiot_parse_id:get_productid(<<"2442284cf5">>, S7_type, ProductName),
%%    case dgiot_parsex:get_object(<<"Product">>, ProductId) of
%%        {ok, #{<<"objectId">> := ObjectId} = Product} ->
%%            dgiot_product:save(Product),
%%            {ok, ObjectId};
%%        _ ->
%%            case dgiot_parsex:create_object(<<"Product">>, ProductBody) of
%%                {ok, #{<<"objectId">> := ObjectId}} ->
%%                    dgiot_product:save(ProductBody#{<<"objectId">> => ObjectId}),
%%                    {ok, ObjectId};
%%                {error, Reason} ->
%%                    {error, Reason}
%%            end
%%    end.

%%get_app(Products) ->
%%    lists:map(fun({ProductId, #{<<"ACL">> := Acl}}) ->
%%        Predicate = fun(E) ->
%%            case E of
%%                <<"role:", _/binary>> -> true;
%%                _ -> false
%%            end
%%                    end,
%%        App =
%%            case lists:filter(Predicate, maps:keys(Acl)) of
%%                [<<"role:", Name/binary>> | _] ->
%%                    Name;
%%                _ ->
%%                    <<"dgiot">>
%%            end,
%%        {ProductId, App}
%%              end, Products).
