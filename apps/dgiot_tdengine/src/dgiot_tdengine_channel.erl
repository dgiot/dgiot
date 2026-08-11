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

-export([read_productid_from_ets/1, new_productid_ets/0]).

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
                        enum => [#{<<"value">> => <<"ChannelId">>, <<"label">> => <<"通道ID"/utf8>>},
                                 #{<<"value">> => <<"ProductId">>, <<"label">> => <<"产品ID"/utf8>>}],
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
                         default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/td_channel.png">>,
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
    dgiot_data:init(?DGIOT_TD_THING_ETS),
    new_productid_ets().


start(ChannelId,
      #{
        <<"ip">> := Ip,
        <<"port">> := Port,
        <<"username">> := UserName,
        <<"password">> := Password
       } = Cfg) ->
    dgiot_tdengine_http:start(),
    Keep = min(maps:get(<<"keep">>, Cfg, 365 * 5), 365 * 5),
    dgiot_channelx:add(?TYPE,
                       ChannelId,
                       ?MODULE,
                       Cfg#{
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
    % 立即发送 init 消息，确保通道启动后立刻创建表
    self() ! init,
    erlang:send_after(1000, self(), ws_login),
    {ok, State}.


handle_event(_EventType, _Event, State) ->
    {ok, State}.


%% gun监测 开始
handle_message({gun_up, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    {ok, State};

handle_message({gun_error, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    {ok, State};

handle_message({gun_down, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    {ok, State};
%% gun监测结束

handle_message(ws_login, #state{id = ChannelId, env = Env} = State) ->
    erlang:send_after(5000, self(), init),
    case dgiot_tdengine_pool:login(ChannelId, Env) of
        {ok, {ConnPid, StreamRef}} ->
            {ok, State#state{env = Env#{<<"driver">> => <<"WS">>, <<"ws_pid">> => ConnPid, <<"ws_ref">> => StreamRef}}};
        {error, Error} ->
            dgiot_bridge:send_log(ChannelId, "Tdengine WS Login error, ~p~n", [Error]),
            {ok, State}
    end;

handle_message(init, #state{id = ChannelId, env = Config} = State) ->
    dgiot_data:insert({?TYPE, ChannelId, config}, Config),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _, ProductIds} ->
            NewProducts = lists:foldl(fun(X, Acc) ->
                                              %% 处理tuple格式的ProductId: {ProductId, _}
                                              ProductId = case X of
                                                  {P, _} when is_binary(P) -> P;
                                                  P when is_binary(P) -> P;
                                                  _ ->
                                                      ?LOG(error, "产品ID格式错误: ~p", [X]),
                                                      undefined
                                              end,
                                              case ProductId of
                                                  undefined ->
                                                      Acc;
                                                  _ ->
                                                      dgiot_data:insert({tdchannel_product, binary_to_atom(ProductId)}, ChannelId),
                                                      save_productid_to_ets(ProductId, ChannelId),
                                                      Acc ++ dgiot_product_tdengine:get_products(ProductId, ChannelId)
                                              end
                                      end,
                                      [],
                                      ProductIds),
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
            dgiot_bridge:send_log(ChannelId, "Save to Tdengine error, ~ts~n, ~p", [unicode:characters_to_list(dgiot_json:encode(Data)), Reason]),
            ok;
        {ok, NewState} ->
            {ok, NewState}
    end;

%% 数据与产品，设备地址分离
handle_message({sql, Sql}, #state{id = ChannelId} = State) ->
    dgiot_metrics:inc(dgiot_tdengine, <<"tdengine_recv">>, 1),
    case catch dgiot_tdengine:batch_sql(ChannelId, Sql) of
        {Err, Reason} when Err == error; Err == 'EXIT' ->
            dgiot_bridge:send_log(ChannelId, "Save to Tdengine error, ~p, ~p", [Sql, Reason]),
            ok;
        _ ->
            pass
    end,
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
    {ok, State};

handle_info({gun_error, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    {ok, State};

handle_info({gun_down, _Pid, _Protocol}, #state{id = _ChannelId, env = _Config} = State) ->
    {ok, State};
%% gun监测结束

handle_info(_Message, State) ->
    {ok, State}.


do_save([ProductId, DevAddr, Data, _Context], State) ->
    dgiot_device:save(ProductId, DevAddr),
    Sql = dgiot_tdengine:format_sql(ProductId, DevAddr, [Data]),
    dgiot_tdengine_adapter:save_sql(ProductId, DevAddr, Sql),
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
              end,
              ProductIds),
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
        {error, Reason} ->
            ?LOG(error, "Create database ~p failed: ~p", [DataBase, Reason]),
            ok
    end.


create_table(ChannelId, ProductId, _Config) ->
    ?LOG(info, ">>> create_table for product ~p", [ProductId]),
    try
        case dgiot_bridge:get_product_info(ProductId) of
            {ok, Product} ->
                Database = dgiot_tdengine:get_database(ChannelId, ProductId),
                TableName = ?Table(ProductId),
                AllColumns = dgiot_tdengine_schema:extract_columns(Product),
                ?LOG(info, ">>> Extracted ~p columns for product ~p", [length(AllColumns), ProductId]),
                case AllColumns of
                    [] ->
                        % 无存储字段 -> 创建包含 dummy 列的最小表，以满足 TDengine 至少一列普通列的要求
                        ?LOG(info, "Product ~p has no storage columns, creating minimal table with dummy column", [ProductId]),
                        BaseSql = <<"CREATE STABLE IF NOT EXISTS ", TableName/binary,
                                    " (createdat TIMESTAMP, dummy INT) TAGS (devaddr NCHAR(64));">>,
                        dgiot_tdengine:batch_sql(ChannelId, Database, BaseSql);
                    _ ->
                        ?LOG(info, ">>> About to call create_stable_by_columns with Database=~p, TableName=~p", [Database, TableName]),
                        Result = dgiot_tdengine_schema:create_stable_by_columns(ChannelId, ProductId, Database, TableName, AllColumns),
                        ?LOG(info, ">>> create_stable_by_columns result for product ~p: ~p", [ProductId, Result])
                end;
            {error, Reason} ->
                ?LOG(error, ">>> Failed to get product info for ~p: ~p", [ProductId, Reason])
        end
    catch
        Class:CatchReason:Stacktrace ->
            ?LOG(error, ">>> Exception in create_table for product ~p: ~p:~p~n~p", [ProductId, Class, CatchReason, Stacktrace])
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
                  },
                 #{<<"channel">> => <<"aaaaaa">>}},
    dgiot_channelx:do_message(?TYPE, <<"09oqrvmPjr">>, Msg, 30000),
    test(I + 1, Max);
test(_, _) -> ok.


new_productid_ets() ->
    case ets:info(td_product_channel, name) of
        undefined ->
            ets:new(td_product_channel, [bag, public, named_table, {write_concurrency, true}, {read_concurrency, true}, {heir, none}]);
        _ ->
            td_product_channel
    end.


save_productid_to_ets(ProductId, ChannelId) when is_binary(ProductId), is_binary(ChannelId) ->
    try
        % 确保ETS表存在
        new_productid_ets(),
        % 使用ets:insert_new避免并发冲突，如果记录已存在则返回false
        case ets:insert_new(td_product_channel, {ProductId, ChannelId}) of
            true ->
                ?LOG(info, "TDengine通道: 产品ID已保存到ETS - ProductId=~s, ChannelId=~s", [ProductId, ChannelId]),
                ok;
            false ->
                % 记录已存在，直接返回成功
                ?LOG(debug, "TDengine通道: 产品ID已存在于ETS - ProductId=~s", [ProductId]),
                ok
        end
    catch
        _:Error ->
            ?LOG(error, "TDengine通道: ETS插入失败 - ProductId=~s, ChannelId=~s, Error=~p", 
                  [ProductId, ChannelId, Error]),
            {error, Error}
    end;
save_productid_to_ets(ProductId, ChannelId) ->
    ?LOG(error, "TDengine通道: 产品ID或通道ID格式错误 - ProductId=~p, ChannelId=~p", [ProductId, ChannelId]),
    {error, invalid_format}.


read_productid_from_ets(ProductId) ->
    ets:lookup(td_product_channel, ProductId).
