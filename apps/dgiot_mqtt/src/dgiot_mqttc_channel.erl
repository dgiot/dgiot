%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 通道框架，事件处理，消息处理
%%% 将外部数据流输入通道里面，然后交由通道处理的架构
%%% @end
%%% Created : 05. 九月 2019 9:38
%%%-------------------------------------------------------------------
-module(shuwa_mqttc_channel).
-behavior(shuwa_channelx).
-define(TYPE, <<"MQTTC">>).
-author("kenneth").
-record(state, {id, client = disconnect}).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 2,
    title => #{
        zh => <<"MQTT资源通道"/utf8>>
    },
    description => #{
        zh => <<"MQTT资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"address">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1">>,
        title => #{
            zh => <<"主机地址"/utf8>>
        },
        description => #{
            zh => <<"主机地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 1883,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"username">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"test"/utf8>>,
        title => #{
            zh => <<"用户名"/utf8>>
        },
        description => #{
            zh => <<"用户名"/utf8>>
        }
    },
    <<"password">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"test"/utf8>>,
        title => #{
            zh => <<"密码"/utf8>>
        },
        description => #{
            zh => <<"密码"/utf8>>
        }
    },
    <<"ssl">> => #{
        order => 6,
        type => boolean,
        required => true,
        default => false,
        title => #{
            zh => <<"SSL"/utf8>>
        },
        description => #{
            zh => <<"是否使用SSL"/utf8>>
        }
    },
    <<"clean_start">> => #{
        order => 7,
        type => boolean,
        required => true,
        default => false,
        title => #{
            zh => <<"清除会话"/utf8>>
        },
        description => #{
            zh => <<"是否清除会话"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/MQTT.png">>,
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
    shuwa_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, ChannelArgs) ->
    Options = [
        {host, binary_to_list(maps:get(<<"address">>, ChannelArgs))},
        {port, maps:get(<<"port">>, ChannelArgs)},
        {clientid, ChannelId},
        {ssl, maps:get(<<"ssl">>, ChannelArgs, false)},
        {username, binary_to_list(maps:get(<<"username">>, ChannelArgs))},
        {password, binary_to_list(maps:get(<<"password">>, ChannelArgs))},
        {clean_start, maps:get(<<"clean_start">>, ChannelArgs, false)}
    ],

    State = #state{
        id = ChannelId
    },
    Specs = [
        {shuwa_mqtt_client, {shuwa_mqtt_client, start_link, [?MODULE, [State], Options]}, permanent, 5000, worker, [shuwa_mqtt_client]}
    ],
    {ok, State, Specs}.

%% 初始化池子
handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    lager:info("channel ~p, ~p", [EventId, Event]),
    ok.

handle_message(Message, State) ->
    lager:info("channel ~p", [Message]),
    {ok, State}.


stop(ChannelType, ChannelId,_State) ->
    lager:info("channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.


%% mqtt client hook
init([State]) ->
    {ok, State#state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, Client}, #state{id = ChannelId} = State) ->
    case shuwa_bridge:get_products(ChannelId) of
        {ok, _Type, ProductIds} ->
            case ProductIds of
                [] -> pass;
                _ ->
                    lists:map(fun(ProductId) ->
%%                        shuwa_product:load(ProductId),
                        emqtt:subscribe(Client, {<<"bridge/thing/", ProductId/binary,"/#">>, 1}),
                        shuwa_mqtt:subscribe(<<"forward/thing/", ProductId/binary, "/+/post">>),
                        shuwa_mqtt:publish(ChannelId, <<"thing/", ProductId/binary>>, jsx:encode(#{<<"network">> => <<"connect">>}))
                              end, ProductIds)
            end,
            lager:info("connect ~p sub ~n", [Client]);
        _ -> pass
    end,
    {noreply, State#state{client = Client}};

handle_info(disconnect, #state{id = ChannelId} = State) ->
    case shuwa_bridge:get_products(ChannelId) of
        {ok, _Type, ProductIds} ->
            case ProductIds of
                [] -> pass;
                _ ->
                    lists:map(fun(ProductId) ->
                        shuwa_mqtt:publish(ChannelId, <<"thing/", ProductId/binary>>, jsx:encode(#{<<"network">> => <<"disconnect">>}))
                              end, ProductIds)
            end;
        _ -> pass
    end,
    {noreply, State#state{client = disconnect}};

handle_info({publish, #{payload := Payload, topic := <<"bridge/", Topic/binary>>} = _Msg}, #state{id = ChannelId} = State) ->
    shuwa_mqtt:publish(ChannelId, Topic, Payload),
    {noreply, State};

handle_info({deliver, _, Msg}, #state{client = Client} = State) ->
    case shuwa_mqtt:get_topic(Msg) of
        <<"forward/", Topic/binary>> ->  emqtt:publish(Client, Topic, shuwa_mqtt:get_payload(Msg));
        _ -> pass
    end,
    {noreply, State};

handle_info(Info, State) ->
    lager:info("unkknow ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
