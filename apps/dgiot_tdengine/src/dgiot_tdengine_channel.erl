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
-export([start/2, check_init/3]).
-export([init/3, handle_event/3, handle_message/2, stop/3, handle_init/1]).
-export([handle_info/2, test/1]).

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
    <<"db">> => #{
        order => 7,
        type => enum,
        default => <<"ProductId"/utf8>>,
        enum => [
            #{<<"value">> => <<"ChannelId">>, <<"label">> => <<"通道ID"/utf8>>},
            #{<<"value">> => <<"ProductId">>, <<"label">> => <<"产品ID"/utf8>>}
        ],
        title => #{
            zh => <<"数据库名称"/utf8>>
        },
        description => #{
            zh => <<"ProductId:用产品ID创建数据库，ChannelId:用通道ID创建数据库"/utf8>>
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
    dgiot_data:init(tdpool),
    dgiot_data:init(?DGIOT_TD_THING_ETS).

start(ChannelId, #{
    <<"ip">> := Ip,
    <<"port">> := Port,
    <<"username">> := UserName,
    <<"password">> := Password
} = Cfg) ->
    dgiot_tdengine_http:start(),
    Keep = min(maps:get(<<"keep">>, Cfg, 365 * 5), 365 * 5),
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, Cfg#{
        <<"keep">> => Keep,
        <<"url">> => list_to_binary(lists:concat(["http://", binary_to_list(Ip), ":", Port, "/rest/sql"])),
        <<"ip">> => dgiot_utils:to_list(Ip),
        <<"port">> => dgiot_utils:to_int(Port),
        <<"username">> => UserName,
        <<"password">> => Password,
        <<"db">> => maps:get(<<"db">>, Cfg, <<"ProductId">>)
    }).

%% 通道初始化
init(?TYPE, ChannelId, Config) ->
    State = #state{
        id = ChannelId,
        env = Config#{<<"driver">> => <<"HTTP">>}
    },
    dgiot_metrics:dec(dgiot_tdengine, <<"tdengine">>, 1000),
    DbType = maps:get(<<"db">>, Config, <<"ProductId">>),
    dgiot_data:insert({tdengine_db, ChannelId}, DbType),
    {ok, State, []}.

handle_init(State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine">>, 1),
    erlang:send_after(1000, self(), ws_login),
    {ok, State}.

handle_event(_EventType, _Event, State) ->
    {ok, State}.

%% gun监测 开始
handle_message({gun_up, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
%%    io:format("~s ~p gun_up = ~p.~n", [?FILE, ?LINE, _Protocol]),
    {ok, State};

handle_message({gun_error, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
%%    io:format("~s ~p gun_error = ~p.~n", [?FILE, ?LINE, _Protocol]),
    {ok, State};

handle_message({gun_down, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
%%    io:format("~s ~p gun_down = ~p.~n", [?FILE, ?LINE, _Protocol]),
    {ok, State};
%% gun监测结束

handle_message(ws_login, #state{id = ChannelId, env = Env} = State) ->
    erlang:send_after(5000, self(), init),
    case dgiot_tdengine_pool:login(ChannelId, Env) of
        {ok, {ConnPid, StreamRef}} ->
            {ok, State#state{env = Env#{<<"driver">> => <<"WS">>, <<"ws_pid">> => ConnPid, <<"ws_ref">> => StreamRef}}};
        {error, _Error} ->
%%            dgiot_bridge:send_log(ChannelId, "Tdengine WS Login error, ~p~n", [Error]),
            {ok, State}
    end;

handle_message(init, #state{id = ChannelId, env = Config} = State) ->
    dgiot_data:insert({?TYPE, ChannelId, config}, Config),
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
handle_message({data, Product, DevAddr, Data, Context}, #state{id = ChannelId} = State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine_recv">>, 1),
    case catch do_save([Product, DevAddr, Data, Context], State) of
        {Err, Reason} when Err == error; Err == 'EXIT' ->
            ?LOG(error, "Save to Tdengine error, ~p, ~p", [Data, Reason]),
            dgiot_bridge:send_log(ChannelId, "Save to Tdengine error, ~ts~n, ~p", [unicode:characters_to_list(jsx:encode(Data)), Reason]),
            ok;
        {ok, NewState} ->
            {ok, NewState}
    end;

%% 数据与产品，设备地址分离
handle_message({sql, Sql}, #state{id = ChannelId} = State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine_recv">>, 1),
    dgiot_tdengine:batch_sql(ChannelId,  Sql),
    {ok, State};

%% 规则引擎导入
handle_message({rule, Msg, Context}, State) ->
    handle_message({data, Msg, Context}, State);

handle_message({sync_product, <<"Product">>, ObjectId}, #state{id = ChannelId, env = Config} = State) ->
    do_check(ChannelId, [ObjectId], Config),
    {ok, State};

handle_message(Message, #state{id = ChannelId, product = ProductId} = _State) ->
    ?LOG(debug, "Channel ~p, Product ~p, handle_message ~p", [ChannelId, ProductId, Message]),
    ok.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

%% gun监测 开始
handle_info({gun_up, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    io:format("~s ~p gun_up = ~p.~n", [?FILE, ?LINE, _Protocol]),
    {ok, State};

handle_info({gun_error, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    io:format("~s ~p gun_error = ~p.~n", [?FILE, ?LINE, _Protocol]),
    {ok, State};

handle_info({gun_down, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    io:format("~s ~p gun_down = ~p.~n", [?FILE, ?LINE, _Protocol]),
    {ok, State};
%% gun监测结束

handle_info(Message, State) ->
    io:format("~s ~p Message = ~p.~n", [?FILE, ?LINE, Message]),
    {ok, State}.

do_save([ProductId, DevAddr, Data, _Context], #state{id = ChannelId} = State) ->
    dgiot_device:save(ProductId, DevAddr),
    Object = dgiot_tdengine:format_data(ChannelId, ProductId, DevAddr, Data),
    dgiot_tdengine:batch(ChannelId, Object),
    {ok, State}.

do_check(ChannelId, ProductIds, Config) ->
    spawn(
        fun() ->
            timer:sleep(500),
            check_init(ChannelId, ProductIds, Config)
        end).

check_init(ChannelId, ProductIds, Config) ->
    lists:map(fun(ProductId) ->
        timer:sleep(500),
        dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
        Id =
            case dgiot_data:get({tdengine_db, ChannelId}) of
                <<"ProductId">> ->
                    ProductId;
                _ ->
                    ChannelId
            end,
        DataBase = dgiot_tdengine_select:format_db(?Database(Id)),
        dgiot_data:insert({tdengine_db, ChannelId, ProductId}, DataBase),
        check_database(ChannelId, ProductId, Config#{<<"database">> => ?Database(Id)})
              end, ProductIds),
    ok.

check_database(ChannelId, ProductId, #{<<"database">> := DataBase, <<"keep">> := Keep} = Config) ->
    case dgiot_tdengine:create_database(ChannelId, DataBase, Keep) of
        {ok, _} ->
            ?LOG(debug, "Check database ChannelId:~p, ProductId:~p, Config:~p", [ChannelId, ProductId, Config]),
            create_table(ChannelId, ProductId, Config);
        {error, <<"channel not find">>} ->
            ok;
        {error, #{<<"code">> := 10, <<"desc">> := <<"authentication failure">>}} ->
            dgiot_bridge:send_log(ChannelId, "Check database Error, ChannelId:~p, ProductId:~p, Reason:authentication failure", [ChannelId, ProductId]),
            timer:sleep(5000),
            check_database(ChannelId, ProductId, Config);
        _ ->
            ok
    end.

create_table(ChannelId, ProductId, _Config) ->
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
%%                            io:format("~p ~p Desc ~p ~n Schema = ~p.~n", [?FILE, ?LINE, _Desc, Schema#{<<"tableName">> => TableName}]),
                            ok;
                        {ok, #{<<"code">> := Code, <<"desc">> := Desc}} ->
                            ?LOG(debug, "Create Table[~s] failed, Code:~p Desc:~p", [TableName, Code, Desc])
                    end
            end;
        {error, Reason} ->
            ?LOG(error, "Create Table Error, ~p ~p", [Reason, ProductId])
    end.

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
