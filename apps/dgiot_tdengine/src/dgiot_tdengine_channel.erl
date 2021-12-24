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

-module(dgiot_tdengine_channel).
-author("kenneth").
-behavior(dgiot_channelx).
-define(CACHE(Channel), binary_to_atom(<<?TYPE/binary, Channel/binary>>, utf8)).
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("kenneth").
-record(state, {id, env, product, status}).
-dgiot_data("ets").
-export([init_ets/0]).
%% API
-export([start/2, transaction/2, run_sql/3, handle_save/1, save_to_cache/2]).
-export([init/3, handle_event/3, handle_message/2, stop/3, handle_init/1]).
-export([test/1]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"TD资源通道"/utf8>>
    },
    description => #{
        zh => <<"TD资源通道"/utf8>>
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
        default => 6041,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"keep">> => #{
        order => 3,
        type => integer,
        required => true,
        default => 365,
        title => #{
            zh => <<"数据保留时间"/utf8>>
        },
        description => #{
            zh => <<"数据保留时间"/utf8>>
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
        default => <<"taosdata">>,
        title => #{
            zh => <<"密码"/utf8>>
        },
        description => #{
            zh => <<"密码"/utf8>>
        }
    },
    <<"driver">> => #{
        order => 6,
        type => string,
        required => true,
        default => <<"HTTP">>,
        title => #{
            zh => <<"连接方式"/utf8>>
        },
        description => #{
            zh => <<"连接方式包括HTTP请求或JDBC"/utf8>>
        }
    },
    <<"os">> => #{
        order => 7,
        type => enum,
        default => <<"linux"/utf8>>,
        enum => [<<"linux">>, <<"windows">>, <<"all">>],
        title => #{
            zh => <<"指定操作系统"/utf8>>
        },
        description => #{
            zh => <<"all:不指定操作系统，自动判断, windows:指定为windows系统，linux:指定为linux系统"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/product/dgiot/channel/TD%E5%9B%BE%E6%A0%87.png">>,
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
    dgiot_data:init(?DGIOT_TD_THING_ETS).

start(ChannelId, #{
    <<"ip">> := Ip,
    <<"port">> := Port,
    <<"driver">> := Driver0,
    <<"username">> := UserName,
    <<"password">> := Password
} = Cfg) ->
    dgiot_tdrestful:start(),
    {Driver, Url} =
        case list_to_binary(string:uppercase(binary_to_list(Driver0))) of
            <<"JDBC">> ->
                {<<"JDBC">>, list_to_binary(lists:concat(["jdbc:TAOS://", binary_to_list(Ip), ":", Port, "/", binary_to_list(<<"dgiot">>)]))};
            _ ->
                {<<"HTTP">>, list_to_binary(lists:concat(["http://", binary_to_list(Ip), ":", Port, "/rest/sql"]))}
        end,
    Keep = min(maps:get(<<"keep">>, Cfg, 365 * 5), 365 * 5),
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, #{
        <<"driver">> => Driver,
        <<"keep">> => Keep,
        <<"url">> => Url,
        <<"username">> => UserName,
        <<"password">> => Password,
        <<"os">> => maps:get(<<"os">>, Cfg, <<"windows">>)
    }).


%% 通道初始化
init(?TYPE, ChannelId, Config) ->
    Opts = [?CACHE(ChannelId), #{
        auto_save => application:get_env(dgiot_tdengine, cache_auto_save, 30000),
        size => application:get_env(dgiot_tdengine, cache_max_size, 50000),
        memory => application:get_env(dgiot_tdengine, cache_max_memory, 102400),
        max_time => application:get_env(dgiot_tdengine, cache_max_time, 30),
        handle => {?MODULE, handle_save, [ChannelId]}
    }],
    State = #state{
        id = ChannelId,
        env = Config
    },
    Specs = [
        {dgiot_dcache, {dgiot_dcache, start_link, Opts}, permanent, 5000, worker, [dgiot_dcache]}
    ],
    dgiot_metrics:dec(dgiot_tdengine, <<"tdengine">>, 1000),
    {ok, State, Specs}.

handle_init(State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine">>, 1),
    erlang:send_after(5000, self(), init),
    dgiot_parse:subscribe(<<"product_id">>, delete),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(full, _From, #state{id = Channel}) ->
    dgiot_dcache:save_to_disk(?CACHE(Channel)),
    ok;

handle_event(EventType, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventType, Event]),
    ok.

handle_message({sync_parse, Args}, State) ->
%%    io:format("ProductArgs ~p~n", [jsx:decode(Args, [{labels, binary}, return_maps])]),
    case jsx:decode(Args, [return_maps]) of
        #{<<"objectId">> := ProductId} ->
            case dgiot_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"key">> => ProductId, <<"class">> => <<"Product">>}}) of
                {ok, #{<<"results">> := Dicts}} ->
                    DictRequests =
                        lists:foldl(fun(#{<<"objectId">> := DictId}, Acc) ->
                            Acc ++ [#{
                                <<"method">> => <<"DELETE">>,
                                <<"path">> => <<"/classes/Dict/", DictId/binary>>,
                                <<"body">> => #{}
                            }]
                                    end, [], Dicts),
                    dgiot_parse:batch(DictRequests);
                _ ->
                    pass
            end,
            case dgiot_parse:query_object(<<"View">>, #{<<"where">> => #{<<"key">> => ProductId, <<"class">> => <<"Product">>}}) of
                {ok, #{<<"results">> := Views}} ->
                    ViewRequests =
                        lists:foldl(fun(#{<<"objectId">> := ViewId}, Acc) ->
                            Acc ++ [#{
                                <<"method">> => <<"DELETE">>,
                                <<"path">> => <<"/classes/View/", ViewId/binary>>,
                                <<"body">> => #{}
                            }]
                                    end, [], Views),
                    io:format("ViewRequests ~p~n", [ViewRequests]),
                    dgiot_parse:batch(ViewRequests);
                _ ->
                    pass
            end;
        _ ->
            pass
    end,
    {ok, State};

%% 规则引擎导入
handle_message({rule, Msg, Context}, State) ->
    ?LOG(info, "Msg ~p", [Msg]),
    ?LOG(info, "Context ~p", [Context]),
    handle_message({data, Msg, Context}, State);


handle_message(init, #state{id = ChannelId, env = Config} = State) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _, ProductIds} ->
            NewProducts = lists:foldl(fun(X, Acc) ->
                Acc ++ dgiot_tdengine:get_products(X, ChannelId)
                                      end, [], ProductIds),
            do_check(ChannelId, dgiot_utils:unique_1(NewProducts), Config),
            {ok, State#state{product = NewProducts}};
        {error, not_find} ->
            {ok, State}
    end;

%% 数据与产品，设备地址分离
handle_message({data, Product, DevAddr, Data, Context}, State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine_recv">>, 1),
    case catch do_save([Product, DevAddr, Data, Context], State) of
        {Err, Reason} when Err == error; Err == 'EXIT' ->
            ?LOG(error, "Save to Tdengine error, ~p, ~p", [Data, Reason]),
            ok;
        {ok, NewState} ->
            {ok, NewState}
    end;

handle_message(config, #state{env = Config} = State) ->
    {reply, {ok, Config}, State};
handle_message(Message, #state{id = ChannelId, product = ProductId} = _State) ->
    ?LOG(info, "Channel ~p, Product ~p, handle_message ~p", [ChannelId, ProductId, Message]),
    ok.

handle_save(Channel) ->
    %{_Time, _} = timer:tc(fun() -> save_cache(Channel) end),
    %?LOG(info,"save:~p~n", [Time / 1000000]),
    save_cache(Channel),
    ok.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

do_save([ProductId, DevAddr, Data, _Context], #state{id = ChannelId} = State) ->
    case dgiot_bridge:get_product_info(ProductId) of
        {error, Reason} ->
            ?LOG(error, "Save to tdengine error, ~p, ~p", [Data, Reason]);
        {ok, #{<<"thing">> := Properties}} ->
            Object = format_data(ProductId, DevAddr, Properties, Data),
            dgiot_device:save(ProductId, DevAddr),
            save_to_cache(ChannelId, Object)
    end,
    {ok, State}.

%% 产品，设备地址与数据分离，推荐
format_data(ProductId, DevAddr, Properties, Data) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    Values = check_fields(Data, Properties),
%%    Fields = lists:foldl(fun(X, Acc) ->
%%        Acc ++ [list_to_binary(string:to_lower(binary_to_list(X)))]
%%                         end, [], maps:keys(Values)),
    Fields = get_fields(ProductId),

    dgiot_data:insert({td, ProductId, DeviceId}, Values#{<<"createdat">> => dgiot_datetime:nowstamp()}),
    Now = maps:get(<<"createdat">>, Data, now),
    NewValues = get_values(ProductId, Values, Now),
    dgiot_data:insert(?DGIOT_TD_THING_ETS, DeviceId, Values),
    #{
        <<"db">> => ?Database(ProductId),
        <<"tableName">> => ?Table(DeviceId),
        <<"using">> => ?Table(ProductId),
        <<"tags">> => [?Table(DevAddr)],
        <<"fields">> => Fields,
        <<"values">> => NewValues
    }.

%% INSERT INTO _173acf2f85._af6d16f9ba using _173acf2f85._173acf2f85 TAGS ('_KOHbyuiJilnsdD') VALUES (now,null,null,null,null);
get_values(ProductId, Values, Now) ->
    Values0 =
        case dgiot_data:get({ProductId, describe_table}) of
            Results when length(Results) > 0 ->
                lists:foldl(fun(Column, Acc) ->
                    case Column of
                        #{<<"Note">> := <<"TAG">>} ->
                            Acc;
                        #{<<"Field">> := <<"createdat">>} ->
                            Acc ++ dgiot_utils:to_list(Now);
                        #{<<"Field">> := Field} ->
                            Value = maps:get(Field, Values, null),
                            case Value of
                                {NewValue, text} ->
                                    Acc ++ ",\'" ++ dgiot_utils:to_list(NewValue) ++ "\'";
                                _ ->
                                    Acc ++ "," ++ dgiot_utils:to_list(Value)
                            end;
                        _ ->
                            Acc
                    end
                            end, " (", Results);
            _ ->
                " "
        end,
    list_to_binary(Values0 ++ ")").


get_fields(ProductId) ->
    case dgiot_data:get({ProductId, describe_table}) of
        Results when length(Results) > 0 ->
            lists:foldl(fun(Column, Acc) ->
                case Column of
                    #{<<"Field">> := Field} ->
                        Acc ++ [Field];
                    _ ->
                        Acc
                end
                        end, [], Results);
        _ ->
            []
    end.

save_cache(Channel) ->
    Max = 50,
    Fun =
        fun({Idx, Requests}, Acc) ->
            true = dgiot_dcache:delete(?CACHE(Channel), Idx),
            save_cache(Channel, Max, Requests, Acc)
        end,
    save_to_tdengine(Channel, dgiot_dcache:search(?CACHE(Channel), Fun)).

save_cache(Channel, Max, [Request | Requests], Acc) when length(Acc) < Max - 1 ->
    save_cache(Channel, Max, Requests, [Request | Acc]);
save_cache(Channel, Max, [Request | Requests], Acc) when length(Acc) == Max - 1 ->
    ok = save_to_tdengine(Channel, [Request | Acc]),
    save_cache(Channel, Max, Requests, []);
save_cache(_, _, [], Acc) ->
    {true, Acc}.


save_to_tdengine(_, []) -> ok;
save_to_tdengine(Channel, Requests) ->
    case dgiot_tdengine:batch(Channel, Requests) of
        {ok, _Results} ->
%%            ?LOG(info, "Batch ~p-> ~p~n", [length(Requests), _Results]),
            ok;
        {error, Reason} when Reason == timeout; Reason == disconnect ->
            ?LOG(error, "save cache,~p,~p~n", [Requests, Reason]),
%%            save_to_cache(Channel, Requests),
            pass;
        {error, Reason} ->
            ?LOG(error, "save cache,~p,~p~n", [Requests, Reason]),
%%            save_to_cache(Channel, Requests),
            pass
    end.


check_cache(Channel) ->
    Info = dgiot_dcache:info(?CACHE(Channel)),
    {size, Size} = lists:keyfind(size, 1, Info),
    case Size =< 100 of
        true ->
            true;
        false ->
            {memory, Memory} = lists:keyfind(memory, 1, Info),
            MaxSize = application:get_env(dgiot_tdengine, cache_max_size, 1000),
            MaxMemory = application:get_env(dgiot_tdengine, cache_max_memory, 102400),
            Size >= MaxSize orelse Memory >= MaxMemory
    end.

do_check(ChannelId, ProductIds, Config) ->
    spawn(
        fun() ->
            timer:sleep(500),
            OsType =
                case maps:get(<<"os">>, Config, <<"all">>) of
                    <<"all">> ->
                        case os:type() of
                            {win32, _} ->
                                <<"windows">>;
                            _ ->
                                <<"linux">>
                        end;
                    <<"windows">> -> <<"windows">>;
                    _ -> <<"linux">>
                end,
            dgiot_data:insert({tdengine_os, ChannelId}, OsType),
            case OsType of
                <<"windows">> ->
                    pass;
                _ ->
                    check_init(ChannelId, ProductIds, Config)
            end
        end).

check_init(ChannelId, ProductIds, Config) ->
    lists:map(fun(ProductId) ->
        timer:sleep(500),
        dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
        check_database(ChannelId, ProductIds, Config#{<<"database">> => ?Database(ProductId)})
              end, ProductIds),
    ok.

check_database(ChannelId, ProductIds, #{<<"database">> := DataBase, <<"keep">> := Keep} = Config) ->
    case dgiot_tdengine:create_database(ChannelId, DataBase, Keep) of
        {ok, _} ->
            ?LOG(debug, "Check database ChannelId:~p, ProductIds:~p, Config:~p", [ChannelId, ProductIds, Config]),
            create_table(ChannelId, ProductIds, Config);
        {error, <<"channel not find">>} ->
            ok;
        {error, #{<<"code">> := 10, <<"desc">> := <<"authentication failure">>}} ->
            dgiot_bridge:send_log(ChannelId, "Check database Error, ChannelId:~p, ProductIds:~p, Reason:authentication failure", [ChannelId, ProductIds]),
            timer:sleep(5000),
            check_database(ChannelId, ProductIds, Config);
        _ ->
%%            ?LOG(error, "Check database Error, ChannelId:~p, ProductIds:~p, Reason:~p", [ChannelId, ProductIds, Reason]),
%%            dgiot_bridge:send_log(ChannelId, "Check database Error, ChannelId:~p, ProductIds:~p, Reason:~p", [ChannelId, ProductIds, Reason]),
%%            timer:sleep(5000),
%%            check_database(ChannelId, ProductIds, Config)
            ok
    end.

create_table(_, [], _) ->
    ok;
create_table(ChannelId, [ProductId | ProductIds], Config) ->
    case dgiot_bridge:get_product_info(ProductId) of
        {ok, Product} ->
            case get_schema(ChannelId, Product) of
                ignore ->
                    ?LOG(debug, "Create Table ignore, ChannelId:~p, ProductId:~p", [ChannelId, Product]);
                Schema ->
                    TableName = ?Table(ProductId),
                    case dgiot_tdengine:create_schemas(ChannelId, Schema#{
                        <<"tableName">> => TableName
                    }) of
                        {error, Reason} ->
                            ?LOG(error, "Create Table[~s] Fail, Schema:~p, Reason:~p", [TableName, Schema, Reason]);
                        {ok, #{<<"affected_rows">> := _}} ->
                            %% @todo 一个产品只能挂一个TDengine?
                            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
                            ?LOG(debug, "Create Table[~s] Succ, Schema:~p", [TableName, Schema])
                    end
            end;
        {error, Reason} ->
            ?LOG(error, "Create Table Error, ~p ~p", [Reason, ProductId])
    end,
    create_table(ChannelId, ProductIds, Config).

get_schema(_ChannelId, Schema) ->
    case maps:get(<<"thing">>, Schema, <<>>) of
        <<>> ->
            ignore;
        Thing ->
            Properties = maps:get(<<"properties">>, Thing, []),
            case lists:flatten([get_field(Property) || Property <- Properties]) of
                [] ->
                    ignore;
                Columns ->
                    AddrSize = integer_to_binary(50),
                    #{
                        <<"fields">> => Columns,
                        <<"tags">> => [
                            {
                                <<"devaddr">>, #{<<"type">> => <<"NCHAR(", AddrSize/binary, ")">>}
                            }
                        ]
                    }
            end
    end.

%%  https://www.taosdata.com/cn/documentation/taos-sql#data-type
%%  #	类型       	Bytes    说明
%%  1	TIMESTAMP   8        时间戳。缺省精度毫秒，可支持微秒。从格林威治时间 1970-01-01 00:00:00.000 (UTC/GMT) 开始，计时不能早于该时间。（从 2.0.18.0 版本开始，已经去除了这一时间范围限制）
%%  2	INT       	4        整型，范围 [-2^31+1, 2^31-1], -2^31 用作 NULL
%%  3	BIGINT      8        长整型，范围 [-2^63+1, 2^63-1], -2^63 用于 NULL
%%  4	FLOAT       4        浮点型，有效位数 6-7，范围 [-3.4E38, 3.4E38]
%%  5	DOUBLE      8        双精度浮点型，有效位数 15-16，范围 [-1.7E308, 1.7E308]
%%  6	BINARY      自定义    记录单字节字符串，建议只用于处理 ASCII 可见字符，中文等多字节字符需使用 nchar。理论上，最长可以有 16374 字节，但由于每行数据最多 16K 字节，实际上限一般小于理论值。binary 仅支持字符串输入，字符串两端需使用单引号引用。使用时须指定大小，如 binary(20) 定义了最长为 20 个单字节字符的字符串，每个字符占 1 byte 的存储空间，总共固定占用 20 bytes 的空间，此时如果用户字符串超出 20 字节将会报错。对于字符串内的单引号，可以用转义字符反斜线加单引号来表示，即 \’。
%%  7	SMALLINT    2        短整型， 范围 [-32767, 32767], -32768 用于 NULL
%%  8	TINYINT     1        单字节整型，范围 [-127, 127], -128 用于 NULL
%%  9	BOOL       	1        布尔型，{true, false}
%%  10	NCHAR       自定义    记录包含多字节字符在内的字符串，如中文字符。每个 nchar 字符占用 4 bytes 的存储空间。字符串两端使用单引号引用，字符串内的单引号需用转义字符 \’。nchar 使用时须指定字符串大小，类型为 nchar(10) 的列表示此列的字符串最多存储 10 个 nchar 字符，会固定占用 40 bytes 的空间。如果用户字符串长度超出声明长度，将会报错。
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"int">>}}) ->
    {Field, #{<<"type">> => <<"INT">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"image">>}}) ->
    {Field, #{<<"type">> => <<"BIGINT">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"long">>}}) ->
    {Field, #{<<"type">> => <<"BIGINT">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"float">>}}) ->
    {Field, #{<<"type">> => <<"FLOAT">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"date">>}}) ->
    {Field, #{<<"type">> => <<"TIMESTAMP">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"bool">>}}) ->
    {Field, #{<<"type">> => <<"BOOL">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"double">>}}) ->
    {Field, #{<<"type">> => <<"DOUBLE">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"string">>} = Spec}) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 10), 200)),
    {Field, #{<<"type">> => <<"NCHAR(", Size/binary, ")">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"text">>} = Spec}) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    {Field, #{<<"type">> => <<"NCHAR(", Size/binary, ")">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"geopoint">>} = Spec}) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    {Field, #{<<"type">> => <<"NCHAR(", Size/binary, ")">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"enum">>, <<"specs">> := _Specs}}) ->
%%    Size = integer_to_binary(maps:size(Specs)),
    {Field, #{<<"type">> => <<"INT">>}};
get_field(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"struct">>, <<"specs">> := SubFields}}) ->
    [get_field(SubField#{<<"identifier">> => ?Struct(Field, Field1)}) || #{<<"identifier">> := Field1} = SubField <- SubFields].


check_fields(Data, #{<<"properties">> := Props}) -> check_fields(Data, Props);
check_fields(Data, Props) -> check_fields(Data, Props, #{}).
check_fields(Data, Props, Acc) when Data == []; Props == [] -> Acc;
check_fields(Data, [#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType} = Prop | Other], Acc) ->
    LowerField = list_to_binary(string:to_lower(binary_to_list(Field))),
    case check_field(Data, Prop) of
        undefined ->
            check_fields(Data, Other, Acc);
        Value ->
            case list_to_binary(string:to_upper(binary_to_list(Type))) of
                <<"STRUCT">> ->
                    #{<<"specs">> := SubFields} = DataType,
                    Acc2 = lists:foldl(
                        fun(#{<<"identifier">> := Field1} = SubField, Acc1) ->
                            case check_field(Value, SubField) of
                                undefined ->
                                    Acc1;
                                Value1 ->
                                    LowerField1 = list_to_binary(string:to_lower(binary_to_list(Field1))),
                                    Acc1#{?Struct(LowerField, LowerField1) => Value1}
                            end
                        end, Acc, SubFields),
                    check_fields(Data, Other, Acc2);
                _ ->
                    check_fields(Data, Other, Acc#{LowerField => Value})
            end
    end.

check_field(Data, Props) when is_map(Data) ->
    NewData =
        maps:fold(fun(K, V, Acc) ->
            NewK = list_to_binary(string:to_lower(binary_to_list(K))),
            Acc#{NewK => V}
                  end, #{}, Data),
    check_field(maps:to_list(NewData), Props);

check_field(Data, #{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType}) ->
    NewField = list_to_binary(string:to_lower(binary_to_list(Field))),
    case proplists:get_value(NewField, Data) of
        undefined ->
            undefined;
        Value ->
            Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
            NewValue =
                case Type1 of
                    _ when Type1 == <<"INT">>; Type1 == <<"DATE">>; Type1 == <<"SHORT">>; Type1 == <<"LONG">>;Type1 == <<"ENUM">>, is_list(Value) ->
                        round(dgiot_utils:to_int(Value));
                    _ when Type1 == <<"INT">>; Type1 == <<"DATE">>, is_float(Value) ->
                        round(Value);
                    _ when Type1 == <<"INT">>; Type1 == <<"DATE">> ->
                        Value;
                    _ when Type1 == <<"FLOAT">>; Type1 == <<"DOUBLE">> ->
                        Specs = maps:get(<<"specs">>, DataType, #{}),
                        Precision = maps:get(<<"precision">>, Specs, 3),
                        dgiot_utils:to_float(Value / 1, Precision);
                    <<"BOOL">> ->
                        Value;
                    <<"TEXT">> ->
                        {unicode:characters_to_binary(unicode:characters_to_list((Value))), text};
                    <<"GEOPOINT">> ->
                        {unicode:characters_to_binary(unicode:characters_to_list((Value))), text};
%%                    <<"ENUM">> ->
%%%%                        Specs = maps:get(<<"specs">>, DataType, #{}),
%%%%                        ?LOG(info, "Specs ~p", [Specs]),
%%%%                        EnumValue = maps:get(Value, Specs, <<"1">>),
%%%%                        dgiot_utils:to_int(EnumValue);
%%                        Value;
                    <<"STRUCT">> ->
                        Value;
                    <<"IMAGE">> ->
                        round(dgiot_utils:to_int(Value));
                    _ ->
                        Value
                end,
            case check_validate(NewValue, DataType) of
                true ->
                    NewValue;
                false ->
                    throw({error, <<Field/binary, " is not validate">>})
            end
    end;

check_field(Data, Props) ->
    ?LOG(error, "Data ~p", [Data]),
    ?LOG(error, "Props ~p", [Props]),
    undefined.

check_validate(Value, #{<<"max">> := Max, <<"min">> := Min}) when is_integer(Max), is_integer(Min) ->
    Value =< Max andalso Value >= Min;
check_validate(Value, #{<<"max">> := Max}) when is_integer(Max) ->
    Value =< Max;
check_validate(Value, #{<<"min">> := Min}) when is_integer(Min) ->
    Value >= Min;
check_validate(_, _) ->
    true.


transaction(Channel, Fun) ->
    case dgiot_channelx:call(?TYPE, Channel, config) of
        {ok, Context} ->
            Fun(Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% Action 用来区分数据库操作语句类型(DQL、DML、DDL、DCL)
run_sql(#{<<"driver">> := <<"HTTP">>, <<"url">> := Url, <<"username">> := UserName, <<"password">> := Password} = Context, _Action, Sql) ->
    ?LOG(debug, " ~p, ~p, ~p, (~ts)", [Url, UserName, Password, unicode:characters_to_list(Sql)]),
    case dgiot_tdrestful:request(Url, UserName, Password, Sql) of
        {ok, Result} ->
            case maps:get(<<"channel">>, Context, <<"">>) of
                <<"">> ->
                    ?LOG(debug, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), Result]);
                ChannelId ->
                    dgiot_bridge:send_log(ChannelId, "Execute ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), jsx:encode(Result)])
            end,
            {ok, Result};
        {error, Reason} ->
            ?LOG(info, "Execute Fail ~p (~ts) ~p", [Url, unicode:characters_to_list(Sql), Reason]),
            {error, Reason}
    end;
run_sql(#{<<"driver">> := <<"JDBC">>, <<"url">> := _Url}, Action, Sql) when Action == execute_update; Action == execute_query ->
%%    ?LOG(info,"Execute ~p (~p) ~p", [Url, byte_size(Sql), Sql]),
    apply(ejdbc, Action, [<<"com.taosdata.jdbc.TSDBDriver">>, Sql]).


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
