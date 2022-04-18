%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 四月 2020 13:34
%%%-------------------------------------------------------------------
-module(dgiot_mysql_channel).
-author("kenneth").
-behavior(dgiot_channelx).
-define(CACHE(Channel), binary_to_atom(<<?TYPE/binary, Channel/binary>>, utf8)).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_mysql.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("kenneth").
-record(state, {id, env, conn, product, status}).
%% API
-export([start/2, transaction/2, handle_save/1, save_to_cache/2]).
-export([init/3, handle_event/3, handle_message/2, stop/3, handle_init/1]).
-export([test/1]).

%% 注册通道类型
-channel(?TYPE).
%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"MYSQL通道"/utf8>>
    },
    description => #{
        zh => <<"MYSQL通道"/utf8>>
    }
}).
-channel_type(#{
    type => 2,
    title => #{
        zh => <<"MYSQL存储通道"/utf8>>
    },
    description => #{
        zh => <<"MYSQL存储通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ip">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1">>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 3306,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"database">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"test">>,
        title => #{
            zh => <<"数据库名称"/utf8>>
        },
        description => #{
            zh => <<"数据库名称"/utf8>>
        }
    },
    <<"username">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"root">>,
        title => #{
            zh => <<"用户名"/utf8>>
        },
        description => #{
            zh => <<"用户名"/utf8>>
        }
    },
    <<"password">> => #{
        order => 5,
        type => string,
        required => true,
        default => <<"root">>,
        title => #{
            zh => <<"密码"/utf8>>
        },
        description => #{
            zh => <<"密码"/utf8>>
        }
    },
    <<"query_timeout">> => #{
        order => 6,
        type => integer,
        required => false,
        default => 5,
        title => #{
            zh => <<"查询超时"/utf8>>
        },
        description => #{
            zh => <<"查询超时"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/mysql.png">>,
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

transaction(Channel, Fun) ->
    case dgiot_channelx:call2(?TYPE, Channel, pool) of
        {ok, Pid} ->
            Fun(Pid);
        {error, Reason} ->
            {error, Reason}
    end.


%% 先缓存定时存库
save_to_cache(Channel, Requests) when is_list(Requests) ->
    ETS = ?CACHE(Channel),
    Key = erlang:system_time(millisecond),
    NewRequest =
        case dgiot_dcache:lookup(ETS, Key) of
            {ok, Acc} ->
                lists:foldl(fun(Request, Acc1) -> [Request | Acc1] end, Acc, Requests);
            {error, not_find} ->
                Requests
        end,
    dgiot_dcache:insert(?CACHE(Channel), {Key, NewRequest}),
    case check_cache(Channel) of
        true ->
            dgiot_channelx:do_event(?TYPE, Channel, full, self()),
            ok;
        false ->
            ok
    end;
save_to_cache(Channel, Request) when is_map(Request) ->
    save_to_cache(Channel, [Request]).


start(ChannelId, #{
    <<"ip">> := Host,
    <<"port">> := Port,
    <<"database">> := Database,
    <<"username">> := UserName,
    <<"password">> := Password,
    <<"Size">> := Size,
    <<"MaxOverFlow">> := MaxOverFlow
} = Cfg) ->
    QueryTimeout = maps:get(<<"query_timeout">>, Cfg, 5),
    Options = [
        {host, binary_to_list(Host)},
        {port, Port},
        {user, binary_to_list(UserName)},
        {password, binary_to_list(Password)},
        {database, binary_to_list(Database)},
        {keep_alive, true},
        {query_timeout, QueryTimeout * 1000},
        {connect_timeout, 30 * 1000}
    ],
    Args = #{
        <<"opts">> => Options,
        <<"Size">> => Size,
        <<"MaxOverFlow">> => MaxOverFlow
    },
    case maps:get(<<"id">>, Cfg, <<>>) of
        <<>> ->
            dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, Args);
        ServerName ->
            dgiot_channelx:add2(ServerName, ?TYPE, ChannelId, ?MODULE, Args)
    end.


%% 通道初始化
init(?TYPE, ChannelId, #{ <<"opts">> := Options}) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _, ProductIds} ->
            Opts = [?CACHE(ChannelId), #{
                auto_save => application:get_env(dgiot_mysql, cache_auto_save, 30000),
                size => application:get_env(dgiot_mysql, cache_max_size, 50000),
                memory => application:get_env(dgiot_mysql, cache_max_memory, 102400),
                max_time =>  application:get_env(dgiot_mysql, cache_max_time, 30),
                handle => {?MODULE, handle_save, [ChannelId]}
            }],
            State = #state{
                id = ChannelId,
                env = Options,
                product = ProductIds
            },
            Specs = [
                {dgiot_dcache, {dgiot_dcache, start_link, Opts}, permanent, 5000, worker, [dgiot_dcache]}
            ],
            do_check(ChannelId, ProductIds),
            {ok, State, Specs};
        {error, not_find} ->
            {stop, not_find_product}
    end.


handle_init(State) ->
    process_flag(trap_exit, true),
    self() ! start_connect,
    {ok, State#state{status = waitting}}.


%% 通道消息处理,注意：进程池调用
handle_event(full, _From, #state{id = Channel}) ->
    dgiot_dcache:save_to_disk(?CACHE(Channel)),
    ok;

handle_event(EventType, Event, _State) ->
    ?LOG(info,"channel ~p, ~p", [EventType, Event]),
    ok.

%% 连接mysql
handle_message(start_connect, #state{id = ChannelId, env = Opts} = State) ->
    case connect(Opts) of
        {ok, Pid} ->
            ?LOG(info,"[MYSQL] start connect SUCC, Config:~p -> ~p", [Opts, Pid]),
            {ok, State#state{status = ready, conn = Pid}};
        {error, _Reason} ->
            dgiot_bridge:send_log(ChannelId, "[MYSQL] start connect error, ~s", [_Reason]),
            {ok, State#state{status = waitting}}
    end;

handle_message({'EXIT', _Pid, Reason}, #state{env = Opts} = State) ->
    ?LOG(error,"[MYSQL] start connect error, Config:~p, ~p", [Opts, Reason]),
    erlang:send_after(30000, self(), start_connect),
    {ok, State#state{status = waitting}};


%% 规则引擎导入
handle_message({rule, Msg, Context}, State) ->
    handle_message({data, Msg, Context}, State);

%% 进程消息
handle_message({data, #{<<"type">> := <<"report">>} = Data, Context}, State) ->
    case catch do_save([Data, Context], State) of
        {Err, Reason} when Err == error; Err == 'EXIT' ->
            ?LOG(error,"Save to Tdengine error, ~p, ~p", [Data, Reason]),
            ok;
        {ok, NewState} ->
            {ok, NewState}
    end;

handle_message(pool, #state{status = Status, conn = Conn} = State) ->
    case Status of
        ready ->
            {reply, {ok, Conn}, State};
        waitting ->
            {reply, {error, disconnect}, State}
    end;
handle_message(Message, #state{id = ChannelId, product = ProductId} = _State) ->
    ?LOG(info,"Channel ~p, Product ~p, handle_message ~p", [ChannelId, ProductId, Message]),
    ok.

handle_save(Channel) ->
    %{_Time, _} = timer:tc(fun() -> save_cache(Channel) end),
    %?LOG(info,"save:~p~n", [Time / 1000000]),
    save_cache(Channel),
    ok.

stop(ChannelType, ChannelId, _) ->
    dgiot_product:del_handler(ChannelId),
    ?LOG(info,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

connect(Opts) ->
    case mysql:start_link(Opts) of
        {ok, Pid} ->
            {ok, Pid};
        ignore ->
            {error, ignore};
        {error, {{_, {error, econnrefused}}, _}} ->
            {error, econnrefused};
        {error, {_ErrorCode, _, Error}} ->
            {error, Error};
        {error, Reason} ->
            {error, Reason}
    end.

do_check(ChannelId, ProductIds) ->
    spawn(
        fun() ->
            timer:sleep(500),
            check_init(ChannelId, ProductIds)
        end).


%% 存储 对于一个通道对应多个产品时，依次调用所有产品解码器 handle_save
do_save([#{<<"productId">> := ProductId} = Data, _Context], #state{id = ChannelId} = State) ->
    case dgiot_bridge:get_product_info(ProductId) of
        {error, Reason} ->
            ?LOG(error,"Save to MySQL error, ~p, ~p", [Data, Reason]);
        {ok, #{<<"thing">> := Properties}} ->
            Object = format_data(ChannelId, ProductId, Properties, Data),
            save_to_cache(ChannelId, Object)
    end,
    {ok, State};
do_save([Data, Context], #state{id = ChannelId, env = Env, product = ProductIds} = State) ->
    F =
        fun(ProductId, Data1) ->
            case dgiot_bridge:get_product_info(ProductId) of
                {error, Reason} ->
                    ?LOG(error,"Save to MySQL error, ~p, ~p", [Data1, Reason]);
                {ok, #{<<"thing">> := Thing}} ->
                    Properties = maps:get(<<"properties">>, Thing, []),
                    Object = format_data(ChannelId, ProductId, Properties, Data1),
                    save_to_cache(ChannelId, Object)
            end
        end,
    case dgiot_bridge:apply_channel(ChannelId, ProductIds, handle_save, [Data, Context], Env) of
        {ok, NewEnv} ->
            case ProductIds of
                [ProductId] ->
                    F(ProductId, Data);
                _ ->
                    ok
            end,
            {ok, update_state(NewEnv, State)};
        {reply, ProductId, NewData, NewEnv} ->
            F(ProductId, NewData),
            {ok, update_state(NewEnv, State)}
    end.

update_state(Env, State) ->
    State#state{env = Env}.

format_data(_ChannelId, ProductId, Properties, #{<<"addr">> := Addr} = Data) ->
    Values = check_fields(Data, Properties),
    Fields = maps:keys(Values),
    Now = dgiot_datetime:nowstamp(),
    #{
        <<"tableName">> => ?Table(ProductId),
        <<"fields">> => [<<"addr">>, <<"createAt">> | Fields],
        <<"values">> => [
            [Addr, Now | [maps:get(Key, Values) || Key <- Fields]]
        ]
    }.


save_cache(Channel) ->
    Max = 50,
    Fun =
        fun({Idx, Requests}, Acc) ->
            true = dgiot_dcache:delete(?CACHE(Channel), Idx),
            save_cache(Channel, Max, Requests, Acc)
        end,
    save_to_mysql(Channel, dgiot_dcache:search(?CACHE(Channel), Fun)).



save_cache(Channel, Max, [Request | Requests], Acc) when length(Acc) < Max - 1 ->
    save_cache(Channel, Max, Requests, [Request | Acc]);
save_cache(Channel, Max, [Request | Requests], Acc) when length(Acc) == Max - 1 ->
    ok = save_to_mysql(Channel, [Request | Acc]),
    save_cache(Channel, Max, Requests, []);
save_cache(_, _, [], Acc) ->
    {true, Acc}.


save_to_mysql(_, []) -> ok;
save_to_mysql(Channel, Requests) ->
    case dgiot_mysql:batch(Channel, Requests) of
        ok ->
            %?LOG(info,"Batch ~p-> ~p~n", [length(Requests), Results]),
            ok;
        {error, Reason} when Reason == timeout; Reason == disconnect ->
            save_to_cache(Channel, Requests),
            ok;
        {error, Reason} ->
            ?LOG(error,"save cache,~p,~p~n", [Requests, Reason]),
            %save_to_cache(Channel, Requests),
            ok
    end.


check_cache(Channel) ->
    Info = dgiot_dcache:info(?CACHE(Channel)),
    {size, Size} = lists:keyfind(size, 1, Info),
    case Size =< 100 of
        true ->
            true;
        false ->
            {memory, Memory} = lists:keyfind(memory, 1, Info),
            MaxSize = application:get_env(dgiot_mysql, cache_max_size, 1000),
            MaxMemory = application:get_env(dgiot_mysql, cache_max_memory, 102400),
            Size >= MaxSize orelse Memory >= MaxMemory
    end.

check_init(ChannelId, ProductIds) ->
    create_table(ChannelId, ProductIds),
    ok.

create_table(_, []) ->
    ok;
create_table(ChannelId, [ProductId | ProductIds]) ->
    case dgiot_bridge:get_product_info(ProductId) of
        {ok, Product} ->
            case get_schema(Product) of
                ignore ->
                    ok;
                Schema ->
                    dgiot_product:add_handler(ProductId, ChannelId, 'message.publish', {channel, ?TYPE}),
                    TableName = ?Table(ProductId),
                    case dgiot_mysql:create_schemas(ChannelId, Schema#{
                        <<"tableName">> => TableName
                    }) of
                        ok ->
                            ?LOG(info,"Create Table[~s] Succ, Schema:~p", [TableName, Schema]);
                        {error, {1050, <<"42S01">>, _}} ->
                            dgiot_data:insert({ProductId, ?TYPE}, ChannelId);
                        {error, Reason} ->
                            ?LOG(error,"Create Table[~s] Fail, Schema:~p, Reason:~p", [TableName, Schema, Reason])
                    end
            end;
        {error, Reason} ->
            ?LOG(error,"Create Table Error, ~p", [Reason])
    end,
    create_table(ChannelId, ProductIds).


get_schema(#{<<"thing">> := Thing}) ->
    Properties = maps:get(<<"properties">>, Thing, []),
    case lists:flatten([get_field(Property) || Property <- Properties]) of
        [] ->
            ignore;
        Columns ->
            #{
                <<"fields">> => [{
                    <<"addr">>, #{<<"type">> => <<"VARCHAR(12)">>}
                }, {
                    <<"createAt">>, #{<<"type">> => <<"INT">>}
                } | Columns]
            }
    end.

get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"int">>}}) ->
    {Field, #{<<"type">> => <<"INT">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"float">>}}) ->
    {Field, #{<<"type">> => <<"FLOAT">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"date">>}}) ->
    {Field, #{<<"type">> => <<"DATE">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"bool">>}}) ->
    {Field, #{<<"type">> => <<"BOOL">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"double">>}}) ->
    {Field, #{<<"type">> => <<"DOUBLE">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"string">>} = Spec}) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 10), 40)),
    {Field, #{<<"type">> => <<"VARCHAR(", Size/binary, ")">>}}.


check_fields(Data, #{<<"properties">> := Props}) -> check_fields(Data, Props);
check_fields(Data, Props) -> check_fields(Data, Props, #{}).
check_fields(Data, Props, Acc) when Data == []; Props == [] -> Acc;
check_fields(Data, [#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := _Type} = _DataType} = Prop | Other], Acc) ->
    case check_field(Data, Prop) of
        undefined ->
            check_fields(Data, Other, Acc);
        Value ->
            check_fields(Data, Other, Acc#{Field => Value})
    end.

check_field(Data, Props) when is_map(Data) ->
    check_field(maps:to_list(Data), Props);
check_field(Data, #{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType}) ->
    case proplists:get_value(Field, Data) of
        undefined ->
            undefined;
        Value ->
            Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
            Is =
                case Type1 of
                    _ when Type1 == <<"INT">>; Type1 == <<"DATE">> ->
                        is_integer(Value);
                    _ when Type1 == <<"FLOAT">>; Type1 == <<"DOUBLE">> ->
                        is_float(Value);
                    <<"BOOL">> ->
                        is_boolean(Value);
                    <<"STRING">> ->
                        is_binary(Value);
                    <<"ENUM">> ->
                        #{<<"specs">> := Specs} = DataType,
                        lists:member(Value, maps:keys(Specs));
                    <<"STRUCT">> ->
                        is_map(Value) orelse is_list(Value)
                end,
            case Is of
                true ->
                    case check_validate(Value, DataType) of
                        true ->
                            Value;
                        false ->
                            throw({error, <<Field/binary, " is not validate">>})
                    end;
                false ->
                    throw({error, <<Field/binary, " is not ", Type/binary>>})
            end
    end.

check_validate(Value, #{<<"max">> := Max, <<"min">> := Min}) when is_integer(Max), is_integer(Min) ->
    Value =< Max andalso Value >= Min;
check_validate(Value, #{<<"max">> := Max}) when is_integer(Max) ->
    Value =< Max;
check_validate(Value, #{<<"min">> := Min}) when is_integer(Min) ->
    Value >= Min;
check_validate(_, _) ->
    true.



test(Count) ->
    test(1, Count).

test(I, Max) when I =< Max ->
    Addr = list_to_binary(io_lib:format("~8.10.0B,", [I])),
    Msg = {rule, #{
        <<"addr">> => Addr,
        <<"te">> => 1,
        <<"te1">> => 2.2,
        <<"te2">> => 3.2,
        <<"te3">> => true,
        <<"te4">> => <<"1">>,
        <<"te5">> => <<"zww">>,
        <<"te6">> => 1587917728,
        <<"te7">> => #{
            <<"fasfd">> => 4
        }
    }, #{<<"channel">> => <<"aaaaaa">>}},
    dgiot_channelx:do_message(?TYPE, <<"09oqrvmPjr">>, Msg, 30000),
    test(I + 1, Max);
test(_, _) -> ok.
