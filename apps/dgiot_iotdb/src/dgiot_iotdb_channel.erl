-module(dgiot_iotdb_channel).
-author("dgiot").
-behavior(dgiot_channelx).
-include("dgiot_iotdb.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-record(state, {id, env, product = []}).

-export([start/2, init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).

-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{zh => <<"IoTDB资源通道"/utf8>>},
    description => #{zh => <<"IoTDB REST时序数据库通道"/utf8>>}
}).

-params(#{
    <<"ip">> => #{
        order => 1, type => string, required => true, default => <<"127.0.0.1">>,
        title => #{zh => <<"服务器地址"/utf8>>}, description => #{zh => <<"IoTDB REST服务地址"/utf8>>}
    },
    <<"port">> => #{
        order => 2, type => integer, required => true, default => 18080,
        title => #{zh => <<"REST端口"/utf8>>}, description => #{zh => <<"IoTDB REST端口"/utf8>>}
    },
    <<"username">> => #{
        order => 3, type => string, required => true, default => <<"root">>,
        title => #{zh => <<"用户名"/utf8>>}, description => #{zh => <<"IoTDB用户名"/utf8>>}
    },
    <<"password">> => #{
        order => 4, type => string, required => true, default => <<"root">>,
        title => #{zh => <<"密码"/utf8>>}, description => #{zh => <<"IoTDB密码"/utf8>>}
    },
    <<"root">> => #{
        order => 5, type => string, required => true, default => ?DEFAULT_ROOT,
        title => #{zh => <<"存储根路径"/utf8>>}, description => #{zh => <<"IoTDB层级存储根路径"/utf8>>}
    },
    <<"db">> => #{
        order => 6, type => enum, required => true, default => <<"ProductId">>,
        enum => [#{<<"value">> => <<"ProductId">>, <<"label">> => <<"产品ID">>},
            #{<<"value">> => <<"ChannelId">>, <<"label">> => <<"通道ID">>}],
        title => #{zh => <<"数据库划分"/utf8>>}, description => #{zh => <<"按产品ID或通道ID创建数据库"/utf8>>}
    },
    <<"timeout">> => #{
        order => 7, type => integer, required => true, default => 30000,
        title => #{zh => <<"请求超时"/utf8>>}, description => #{zh => <<"REST请求超时毫秒数"/utf8>>}
    },
    <<"ssl">> => #{
        order => 8, type => boolean, required => false, default => false,
        title => #{zh => <<"启用HTTPS"/utf8>>}, description => #{zh => <<"通过HTTPS访问IoTDB REST服务"/utf8>>}
    },
    <<"ico">> => #{
        order => 102, type => string, required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/iotdb_channel.png">>,
        title => #{zh => <<"通道ICO"/utf8>>}, description => #{zh => <<"通道ICO"/utf8>>}
    }
}).

start(ChannelId, #{<<"ip">> := Ip0, <<"port">> := Port0,
        <<"username">> := UserName, <<"password">> := Password} = Config) ->
    Ip = dgiot_utils:to_binary(Ip0),
    Port = dgiot_utils:to_int(Port0),
    Scheme = case maps:get(<<"ssl">>, Config, false) of true -> <<"https://">>; _ -> <<"http://">> end,
    Url = <<Scheme/binary, Ip/binary, ":", (integer_to_binary(Port))/binary>>,
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE,
        Config#{<<"url">> => Url,
            <<"username">> => dgiot_utils:to_binary(UserName),
            <<"password">> => dgiot_utils:to_binary(Password),
            <<"root">> => maps:get(<<"root">>, Config, ?DEFAULT_ROOT),
            <<"db">> => maps:get(<<"db">>, Config, <<"ProductId">>),
            <<"timeout">> => max(1000, dgiot_utils:to_int(maps:get(<<"timeout">>, Config, 30000)))
        }).

init(?TYPE, ChannelId, Config) ->
    {ok, #state{id = ChannelId, env = Config}, []}.

handle_init(State) ->
    erlang:send_after(500, self(), init_products),
    {ok, State}.

handle_event(_EventType, _Event, State) ->
    {ok, State}.

handle_message(init_products, #state{id = ChannelId, env = Config} = State) ->
    dgiot_data:insert({?TYPE, ChannelId, config}, Config),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _Type, ProductIds} ->
            Products = lists:usort(lists:flatmap(fun(ProductId) ->
                dgiot_product_iotdb:get_products(ProductId, ChannelId)
            end, ProductIds)),
            spawn(fun() -> bootstrap_products(ChannelId, Products) end),
            {ok, State#state{product = Products}};
        {error, not_find} -> {ok, State};
        {error, Reason} ->
            dgiot_bridge:send_log(ChannelId, "IoTDB channel product lookup failed: ~p~n", [Reason]),
            {ok, State}
    end;

handle_message({data, ProductId, DevAddr, Data, _Context}, #state{id = ChannelId} = State)
        when is_map(Data) ->
    save_data(ChannelId, ProductId, DevAddr, Data),
    {ok, State};

handle_message({data, #{<<"productId">> := ProductId} = Data, _Context}, #state{id = ChannelId} = State) ->
    DevAddr = maps:get(<<"devaddr">>, Data, maps:get(<<"addr">>, Data, <<>>)),
    save_data(ChannelId, ProductId, DevAddr, Data),
    {ok, State};

handle_message({sql, Sql}, #state{id = ChannelId} = State) ->
    case dgiot_iotdb:batch_sql(ChannelId, Sql) of
        {error, Reason} -> log_error(ChannelId, "IoTDB SQL execution failed", Sql, Reason);
        _ -> ok
    end,
    {ok, State};

handle_message({rule, #{<<"productId">> := ProductId} = Data, _Context}, State) ->
    DevAddr = maps:get(<<"devaddr">>, Data, maps:get(<<"addr">>, Data, <<>>)),
    handle_message({data, ProductId, DevAddr, Data, #{}}, State);

handle_message({sync_product, <<"Product">>, ProductId}, #state{id = ChannelId, env = _Config} = State) ->
    Products = dgiot_product_iotdb:get_products(ProductId, ChannelId),
    spawn(fun() -> bootstrap_products(ChannelId, Products) end),
    {ok, State#state{product = lists:usort(Products ++ State#state.product)}};

handle_message(Message, #state{id = ChannelId, product = Products} = State) ->
    ?LOG(debug, "IoTDB channel ~p products ~p ignored message ~p", [ChannelId, Products, Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "IoTDB channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

save_data(ChannelId, ProductId, DevAddr, Data) ->
    try
        dgiot_device:save(ProductId, DevAddr),
        case dgiot_iotdb:format_data(ChannelId, ProductId, DevAddr, Data) of
            #{<<"fields">> := Fields} = Record when map_size(Fields) > 0 ->
                case dgiot_iotdb:batch(ChannelId, [Record]) of
                    {error, Reason} -> log_error(ChannelId, "IoTDB write failed", Data, Reason);
                    _ -> ok
                end;
            {error, Reason} -> log_error(ChannelId, "IoTDB record format failed", Data, Reason);
            _ -> ok
        end
    catch
        Class:ExceptionReason -> log_error(ChannelId, "IoTDB save raised exception", Data, {Class, ExceptionReason})
    end.

bootstrap_products(ChannelId, ProductIds) ->
    lists:foreach(fun(ProductId) ->
        Database = dgiot_iotdb:get_database(ChannelId, ProductId),
        case dgiot_iotdb:create_database(ChannelId, Database) of
            {error, Reason} -> log_error(ChannelId, "IoTDB database initialization failed", Database, Reason);
            _ -> ok
        end
    end, ProductIds).

log_error(ChannelId, Label, Detail, Reason) ->
    ?LOG(error, "~s, ~p, ~p", [Label, Detail, Reason]),
    dgiot_bridge:send_log(ChannelId, "~s, ~p, ~p~n", [Label, Detail, Reason]),
    ok.
