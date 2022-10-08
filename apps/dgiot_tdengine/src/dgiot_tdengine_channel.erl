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
-export([start/2, handle_save/1, save_to_cache/2]).
-export([init/3, handle_event/3, handle_message/2, stop/3, handle_init/1]).
-export([test/1]).
-export([check_init/3]).

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
        enum => [
            #{<<"value">> => <<"linux">>, <<"label">> => <<"linux"/utf8>>},
            #{<<"value">> => <<"windows">>, <<"label">> => <<"windows"/utf8>>},
            #{<<"value">> => <<"all">>, <<"label">> => <<"all"/utf8>>}
        ],
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
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/TDIcon.png">>,
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
    {Driver, Url} = dgiot_tdengine_pool:start(list_to_binary(string:uppercase(binary_to_list(Driver0))), Ip, Port),
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

handle_init(#state{id = _ChannelId} = State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine">>, 1),
    erlang:send_after(5000, self(), init),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(full, _From, #state{id = Channel} = State) ->
    dgiot_dcache:save_to_disk(?CACHE(Channel)),
    {ok, State};

handle_event(_EventType, _Event, State) ->
    {ok, State}.

%% 规则引擎导入
handle_message({rule, Msg, Context}, State) ->
    handle_message({data, Msg, Context}, State);

handle_message(init, #state{id = ChannelId, env = Config} = State) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _, ProductIds} ->
            NewProducts = lists:foldl(fun(X, Acc) ->
                Acc ++ dgiot_product_tdengine:get_products(X, ChannelId)
                                      end, [], ProductIds),
            do_check(ChannelId, dgiot_utils:unique_1(NewProducts), Config),
            {ok, State#state{product = NewProducts}};
        {error, not_find} ->
            {ok, State}
    end;

%% 数据与产品，设备地址分离
handle_message({data, Product, DevAddr, Data, Context}, #state{id = ChannelId, env = Config} = State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine_recv">>, 1),
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
            case catch dgiot_parse_timescale:do_save(Product, DevAddr, Data) of
                {ok, _} ->
                    {ok, State};
                {_, Reason} ->
                    ?LOG(error, "Save to Tdengine error, ~p, ~p", [Data, Reason]),
                    dgiot_bridge:send_log(ChannelId, "Save to Tdengine error, ~ts~n, ~p", [unicode:characters_to_list(jsx:encode(Data)), Reason]),
                    ok
            end;
        _ ->
            case catch do_save([Product, DevAddr, Data, Context], State) of
                {Err, Reason} when Err == error; Err == 'EXIT' ->
                    ?LOG(error, "Save to Tdengine error, ~p, ~p", [Data, Reason]),
                    dgiot_bridge:send_log(ChannelId, "Save to Tdengine error, ~ts~n, ~p", [unicode:characters_to_list(jsx:encode(Data)), Reason]),
                    ok;
                {ok, NewState} ->
                    {ok, NewState}
            end
    end;

handle_message(config, #state{env = Config} = State) ->
    {reply, {ok, Config}, State};

handle_message({sync_product, <<"Product">>, ObjectId}, #state{id = ChannelId, env = Config} = State) ->
    do_check(ChannelId, [ObjectId], Config),
    {ok, State};

handle_message(Message, #state{id = ChannelId, product = ProductId} = _State) ->
    ?LOG(debug, "Channel ~p, Product ~p, handle_message ~p", [ChannelId, ProductId, Message]),
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
            ?LOG(error, "Save to tdengine error, ~p, ~p, ProductId = ~p.", [Data, Reason, ProductId]);
        {ok, #{<<"thing">> := Properties}} ->
            Object = dgiot_tdengine:format_data(ProductId, DevAddr, Properties, Data),
            dgiot_device:save(ProductId, DevAddr),
            save_to_cache(ChannelId, Object)
    end,
    {ok, State}.

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
                    dgiot_parse_channel:start_timescale();
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
            ok
    end.

create_table(_, [], _) ->
    ok;
create_table(ChannelId, [ProductId | ProductIds], Config) ->
    case dgiot_bridge:get_product_info(ProductId) of
        {ok, Product} ->
            case dgiot_tdengine_schema:get_schema(ChannelId, Product) of
                ignore ->
                    ?LOG(debug, "Create Table ignore, ChannelId:~p, ProductId:~p", [ChannelId, Product]);
                Schema ->
                    TableName = ?Table(ProductId),
                    case dgiot_tdengine:create_schemas(ChannelId, Schema#{<<"tableName">> => TableName}) of
                        {error, Reason} ->
                            ?LOG(error, "Create Table[~s] Fail, Schema:~p, Reason:~p", [TableName, Schema, Reason]);
                        {ok, #{<<"affected_rows">> := _}} ->
                            %% @todo 一个产品只能挂一个TDengine?
                            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
                            ?LOG(debug, "Create Table[~s] Succ, Schema:~p", [TableName, Schema]);
                        {ok, #{<<"code">> := 0, <<"column_meta">> := _}} ->
                            %% @todo 一个产品只能挂一个TDengine?
                            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
                            ?LOG(debug, "Create Table[~s] Succ, Schema:~p", [TableName, Schema]);
                        {ok, #{<<"code">> := 904, <<"desc">> := _Desc}} ->
%%                            io:format("~p ~p Code: ~p Desc ~p ~n", [?FILE, ?LINE, Code, Desc])
                            ok;
                        {ok, #{<<"code">> := Code, <<"desc">> := Desc}} ->
                            ?LOG(debug, "Create Table[~s] failed, Code:~p Desc:~p", [TableName, Code, Desc])
                    end
            end;
        {error, Reason} ->
            ?LOG(error, "Create Table Error, ~p ~p", [Reason, ProductId])
    end,
    create_table(ChannelId, ProductIds, Config).


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
