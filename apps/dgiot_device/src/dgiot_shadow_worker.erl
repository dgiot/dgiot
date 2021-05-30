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

-module(dgiot_shadow_worker).
-include("dgiot_device.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("kenneth").
-record(state, {mod, productId, devaddr, id, channel = #{}, ms = 0, childState, gateway, status}).


%% API
-export([start_link/3, start_link/4, init/1, handle_info/2, handle_cast/2, code_change/3, terminate/2, handle_call/3]).

-export([add_data/4, get_data/2, lookup/2, is_online/2, get_status/2, get_child/3]).


-callback init(VConName :: any(), Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: term()) ->
    term().


%% 加缓存
add_data(ProductId, DevAddr, Key, Value) ->
    case lookup(ProductId, DevAddr) of
        {ok, Pid, Cache} ->
            dgiot_data:insert(?LOCAL, [ProductId, DevAddr], {Pid, Cache#{Key => Value}});
        {error, not_find} ->
            not_alive
    end.


%% 取缓存
get_data(ProductId, DevAddr) ->
    case lookup(ProductId, DevAddr) of
        {error, not_find} ->
            not_alive;
        {ok, _Pid, Cache} ->
            {ok, Cache}
    end.


%% 是否在线
is_online(ProductId, DevAddr) ->
    case lookup(ProductId, DevAddr) of
        {ok, Pid, _Cache} ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                true ->
                    Pid;
                false ->
                    false
            end;
        {error, not_find} ->
            false
    end.

%% 获取子设备
get_child(ProductId, Gateway, ShortAddr) ->
    case dgiot_data:lookup(?LOCAL, {ProductId, Gateway, ShortAddr}) of
        {ok, Pid} ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    {error, not_alive}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_status(ProductId, DevAddr) ->
    case lookup(ProductId, DevAddr) of
        {ok, Pid, Cache} ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                true ->
                    maps:get(<<"status">>, Cache, <<"OFFLINE">>);
                false ->
                    <<"OFFLINE">>
            end;
        {error, not_find} ->
            <<"OFFLINE">>
    end.


%% 取设备缓存
lookup(ProductId, DevAddr) ->
    case dgiot_data:lookup(?LOCAL, [ProductId, DevAddr]) of
        {ok, {Pid, Cache}} ->
            case is_pid(Pid) andalso is_process_alive(Pid) of
                true ->
                    {ok, Pid, Cache};
                false ->
                    dgiot_data:delete(?LOCAL, [ProductId, DevAddr]),
                    {error, not_find}
            end;
        {error, not_find} ->
            {error, not_find}
    end.


start_link(ProductId, DevAddr, Args) ->
    start_link(undefined, ProductId, DevAddr, Args).


start_link(DevMod, ProductId, DevAddr, Args) ->
    case is_online(ProductId, DevAddr) of
        false ->
            SubTopic = format_sub_topic(ProductId, DevAddr, Args),
            Device = format_device(Args#{<<"topics">> => SubTopic}),
            dgiot_data:insert(?LOCAL, [ProductId, DevAddr], {init, Device}),
            gen_server:start_link(?MODULE, [DevMod, binary:copy(ProductId), binary:copy(DevAddr)], []);
        Pid ->
            {ok, Pid}
    end.


init([DevMod, ProductId, DevAddr]) ->
    State = #state{productId = ProductId, devaddr = DevAddr, mod = DevMod},
    Result = dgiot_data:lookup(?LOCAL, [ProductId, DevAddr]),
    case Result of
        {ok, {init, Device}} ->
            dgiot_data:delete(?LOCAL, [ProductId, DevAddr]),
            case DevMod of
                undefined ->
                    do_start(Device, State);
                DevMod ->
                    case DevMod:init(ProductId, Device) of
                        {ok, ChildState} ->
                            do_start(Device, State#state{childState = ChildState});
                        {stop, Reason} ->
                            {stop, Reason}
                    end
            end;
        {error, not_find} ->
            case dgiot_shadow:lookup(node(), ProductId, DevAddr) of
                {ok, _, Device} ->
                    do_start(Device, State);
                {error, Reason} ->
                    {stop, Reason}
            end
    end.


handle_cast(Msg, State) ->
    case do_worker(handle_cast, [Msg], State) of
        no_module ->
            {noreply, State, hibernate};
        Result ->
            Result
    end.

%% 设备注册
handle_call({register, #{<<"channel">> := ChannelId}}, _From, #state{productId = ProductId, devaddr = DevAddr} = State) ->
    case State#state.status of
        {register, ChannelId} ->
            {reply, {error, waitting}, State, hibernate};
        _ ->
            case dgiot_shadow:lookup(ProductId, DevAddr) of
                {ok, _Node, #dgiot_device{basedata = Device}} ->
                    create_device(ProductId, #{
                        action => register,
                        basedata => Device#{
                            <<"devaddr">> => DevAddr
                        }
                    }),
                    {reply, ok, do_online(ChannelId, State#state{status = {register, ChannelId}}), hibernate};
                {error, Reason} ->
                    {error, Reason}
            end
    end;

%% 设备上线
handle_call({online, #{<<"channel">> := ChannelId} = Message}, _From, #state{devaddr = DevAddr} = State) ->
    BaseData = maps:get(<<"basedata">>, Message, #{}),
    case State#state.status of
        {register, _} ->
            {reply, {error, waitting}, State, hibernate};
        {online, ChannelId} ->
            {reply, ok, State, hibernate};
        _ ->
            save_device(State#state.id, #{
                action => online,
                basedata => BaseData#{
                    <<"status">> => <<"ONLINE">>
                }
            }),
            load_children(DevAddr),
            {reply, ok, do_online(ChannelId, State#state{status = {online, ChannelId}}), hibernate}
    end;

%% 设备离线
handle_call({offline, #{<<"channel">> := ChannelId} = Message}, _From, State) ->
    BaseData = maps:get(<<"basedata">>, Message, #{}),
    case maps:get(ChannelId, State#state.channel, no) of
        no ->
            {reply, ok, State, hibernate};
        true ->
            ?LOG(info,"Offline ~p, ~p", [ChannelId, State#state.devaddr]),
            do_message(<<"notify">>, State#state.devaddr, State#state.productId, #{ <<"status">> => <<"offline">> }),
            Channels = maps:remove(ChannelId, State#state.channel),
            case maps:size(Channels) == 0 of
                true ->
                    Key = [State#state.productId, State#state.devaddr],
                    Self = self(),
                    NewCache =
                        case dgiot_data:lookup(?LOCAL, Key) of
                            {ok, {Self, Cache}} ->
                                maps:remove(<<"status">>, Cache);
                            {error, not_find} ->
                                #{}
                        end,
                    dgiot_data:insert(?LOCAL, Key, {Self, NewCache}),
                    save_device(State#state.id, BaseData#{
                        action => offline,
                        basedata => #{<<"status">> => <<"OFFLINE">>}
                    }),
                    do_children(State,
                        fun(Pid) ->
                            gen_server:call(Pid, {offline, Message}, 5000)
                        end);
                false ->
                    ok
            end,
            {reply, ok, State#state{channel = Channels}, hibernate}
    end;

%% 设备更新
handle_call({update, Device}, _From, State) ->
    save_device(State#state.id, #{
        action => update,
        basedata => Device
    }),
    {reply, ok, State, hibernate};

%% 设备心跳，如果是网关的话，所有子设备设置为在线，
handle_call({event, ChannelId, heart}, _From, State) ->
    ?LOG(info,"Device Event ~p", [heart]),
    {reply, ok, do_online(ChannelId, State), hibernate};

%% 设备事件上报
handle_call({event, _ChannelId, EventId}, _From, State) ->
    ?LOG(info,"Device Event ~p", [EventId]),
    {reply, ok, State, hibernate};

handle_call(Msg, From, State) ->
    case do_worker(handle_call, [Msg, From], State) of
        no_module ->
            {reply, ok, State, hibernate};
        Result ->
            Result
    end.


%% 设备上报
handle_info({report, Data}, #state{productId = ProductId, devaddr = DevAddr} = State) ->
    case dgiot_product:local(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
            Values = check_property(Data, Properties),
            Map = maps:from_list(Values),
            do_message(<<"report">>, DevAddr, ProductId, Map);
        {error, not_find} ->
            ?LOG(error,"not find product ~p", [ProductId])
    end,
    {noreply, State, hibernate};

handle_info({send, Msg, Callback}, State) ->
    catch Callback({ok, Msg}),
    {noreply, State, hibernate};

handle_info({deliver, Topic, Msg}, #state{productId = ProductId, devaddr = DevAddr} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    case catch jsx:decode(Payload, [{labels, binary}, return_maps]) of
        {'EXIT', Reason} ->
            ?LOG(error,"~p, ~p, ~p, ~p~n", [ProductId, DevAddr, Topic, Reason]);
        Message ->
            ?LOG(debug,"~p, ~p, ~p, ~p~n", [ProductId, DevAddr, Topic, Message])
    end,
    {noreply, State, hibernate};


%% 创建成功 -> 注册成功
handle_info({batch, #{action := register, <<"body">> := Body}, Result}, State) ->
    case Result of
        #{<<"success">> := #{<<"objectId">> := Id}} ->
            ?LOG(info,"Register Success ~p -> ~p", [State#state.productId, State#state.devaddr]),
            {noreply, State#state{id = Id, status = ready}, hibernate};
        #{<<"error">> := Error} ->
            ?LOG(error,"Save DB Fail, Body:~p, Reason:~p", [Body, Error]),
            {stop, normal, State}
    end;

%% 更新成功 -> 离线/上线
handle_info({batch, #{action := Action, <<"body">> := Body}, Result}, State) when Action == online; Action == offline ->
    case Result of
        #{<<"success">> := #{<<"updatedAt">> := _updateAt}} ->
            ?LOG(info,"~p Success Product: ~p, Gateway: ~p -> ~p", [Action, State#state.productId, State#state.gateway, State#state.devaddr]),
            {noreply, State#state{status = ready}, hibernate};
        #{<<"error">> := #{<<"code">> := 101,<<"error">> := <<"Object not found.">>} } ->
            {stop, normal, State};
        #{<<"error">> := Error} ->
            ?LOG(error,"Save DB Fail, Body:~p, Reason:~p", [Body, Error]),
            {noreply, State, hibernate}
    end;

%% 更新成功
handle_info({batch, #{action := update, <<"body">> := Body}, Result}, State) ->
    case Result of
        #{<<"success">> := #{<<"updatedAt">> := _updateAt}} ->
            ?LOG(info,"Update Success ~p -> ~p", [State#state.productId, State#state.devaddr]),
            {noreply, State, hibernate};
        #{<<"error">> := #{<<"code">> := 101,<<"error">> := <<"Object not found.">>} } ->
            {stop, normal, State};
        #{<<"error">> := Error} ->
            ?LOG(error,"Save DB Fail, Body:~p, Reason:~p", [Body, Error]),
            {noreply, State, hibernate}
    end;

%% 查询子设备
handle_info({batch, #{action := load_children, <<"body">> := _Body}, Result}, State) ->
    case Result of
        #{<<"success">> := #{<<"results">> := []}} ->
            {noreply, State, hibernate};
        #{<<"success">> := #{<<"results">> := Results}} ->
            Key = [State#state.productId, State#state.devaddr],
            Self = self(),
            NewCache =
                case dgiot_data:lookup(?LOCAL, Key) of
                    {ok, {Self, Cache}} ->
                        Cache;
                    {error, not_find} ->
                        #{}
                end,
            Children = start_children(Results, State),
            dgiot_data:insert(?LOCAL, Key, {Self, NewCache#{<<"children">> => Children}}),
%%            ?LOG(info,"Addr:~p, children:~p", [State#state.devaddr, Children]),
            {noreply, State, hibernate};
        #{<<"error">> := _Error} ->
%%            ?LOG(error,"Query Fail, Body:~p, Reason:~p", [Body, Error]),
            {noreply, State, hibernate}
    end;


handle_info(Info, State) ->
    case do_worker(handle_info, [Info], State) of
        no_module ->
            {noreply, State, hibernate};
        Result ->
            Result
    end.

terminate(Reason, #state{productId = ProductId, devaddr = DevAddr, gateway = Gateway} = State) ->
    dgiot_data:delete(?LOCAL, [ProductId, DevAddr]),
    Gateway =/= undefined andalso dgiot_data:delete(?LOCAL, Gateway),
    case do_worker(terminate, [Reason], State) of
        no_module -> ok;
        Result -> Result
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_worker(_Fun, _Args, #state{mod = undefined}) ->
    no_module;
do_worker(Fun, Args, #state{mod = Mod, childState = ChildState} = State) ->
    case handle(apply(Mod, Fun, Args ++ [ChildState]), State) of
        {'EXIT', Reason} ->
            {stop, Reason, State};
        Result ->
            Result
    end.



handle({noreply, ChildState}, State) ->
    {noreply, State#state{childState = ChildState}, hibernate};

handle({noreply, ChildState, hibernate}, State) ->
    {noreply, State#state{childState = ChildState}, hibernate};

handle({noreply, ChildState, Timeout}, State) ->
    {noreply, State#state{childState = ChildState}, Timeout};

handle({stop, Reason, ChildState}, State) ->
    {stop, Reason, State#state{childState = ChildState}};

handle({reply, Reply, ChildState}, State) ->
    {reply, Reply, State#state{childState = ChildState}, hibernate};

handle({reply, Reply, ChildState, hibernate}, State) ->
    {reply, Reply, State#state{childState = ChildState}, hibernate};

handle({reply, Reply, ChildState, Timeout}, State) ->
    {reply, Reply, State#state{childState = ChildState}, Timeout};

handle({stop, Reason, Reply, ChildState}, State) ->
    {stop, Reason, Reply, State#state{childState = ChildState}};

handle(ok, _State) ->
    ok.


%% ===============================================

do_start(#{<<"nodeType">> := NodeType} = Device, #state{productId = ProductId, devaddr = DevAddr} = State) ->
    case dgiot_shadow:save(ProductId, DevAddr, Device) of
        ok ->
            Cache = #{<<"status">> => <<"OFFLINE">>},
            SubTopic = maps:get(<<"topics">>, Device, []),
            [dgiot_mqtt:subscribe(Topic) || Topic <- SubTopic],
            dgiot_data:insert(?LOCAL, [ProductId, DevAddr], {self(), Cache}),
            Id = maps:get(<<"objectId">>, Device, undefined),
            case NodeType of
                1 ->
                    load_children(DevAddr);
                _ ->
                    ok
            end,
            Gateway = maps:get(<<"gateway">>, Device, undefined),
            case Gateway of
                {Addr, ShortAddr} ->
                    %% 存储子设备对应关系
                    dgiot_data:insert(?LOCAL, {ProductId, Addr, ShortAddr}, self());
                undefined ->
                    ok
            end,
            {ok, State#state{id = Id, gateway = Gateway}, hibernate};
        {error, Reason} ->
            {stop, Reason}
    end.


start_children(Children, State) ->
    start_children(Children, State, []).
start_children([], _, Acc) -> Acc;
start_children([#{<<"product">> := Product, <<"route">> := Route} = Device | Other], #state{ devaddr = Addr} = State, Acc) ->
    %% 存储子设备产品
    {ok, Product1} = dgiot_product:save(Product),
    ShortAddr = maps:get(Addr, Route),
    Device1 = maps:remove(<<"product">>, Device#{
        <<"gateway">> => {Addr, ShortAddr}
    }),
    case dgiot_product:add_device(Product1#{<<"dynamicReg">> => <<"load">>}, Device1) of
        {ok, Pid} ->
            %% @todo
            Pid ! {link, self()},
            start_children(Other, State, [ShortAddr | Acc]);
        {error, Reason} ->
            ?LOG(error,"Product:~p, DevAddr:~p -> ~p", [Product1, Device1, Reason]),
            start_children(Other, State, Acc)
    end.

do_children(#state{productId = ProductId, devaddr = DevAddr}, Fun) ->
    Self = self(),
    NewCache =
        case dgiot_data:lookup(?LOCAL, [ProductId, DevAddr]) of
            {ok, {Self, Cache}} -> Cache;
            {error, not_find} -> #{}
        end,
    case maps:get(<<"children">>, NewCache, []) of
        [] ->
            ok;
        Children ->
            lists:foreach(
                fun(ShortAddr) ->
                    case get_child(ProductId, DevAddr, ShortAddr) of
                        {ok, _, Pid} ->
                            Fun(Pid);
                        {error, Reason} ->
                            ?LOG(error,"~p,~p,~p", [DevAddr, ShortAddr, Reason])
                    end
                end, Children)
    end.


do_online(ChannelIds, State) when is_map(ChannelIds) ->
    maps:fold(
        fun(ChannelId, IsOnline, NewState) ->
            case IsOnline of
                true ->
                    do_online(ChannelId, NewState);
                false ->
                    NewState
            end
        end, State, ChannelIds);
do_online(ChannelId, #state{productId = ProductId, devaddr = DevAddr, channel = Channels} = State) ->
    ?LOG(info,"Product:~p DevAddr:~p Online from ~p", [ProductId, DevAddr, ChannelId]),
    do_message(<<"notify">>, State#state.devaddr, State#state.productId, #{ <<"status">> => <<"online">> }),
    Key = [State#state.productId, State#state.devaddr],
    Self = self(),
    NewCache =
        case dgiot_data:lookup(?LOCAL, Key) of
            {ok, {Self, Cache}} ->
                Cache#{
                    <<"status">> => <<"ONLINE">>
                };
            {error, not_find} ->
                #{
                    <<"status">> => <<"ONLINE">>
                }
        end,
    dgiot_data:insert(?LOCAL, Key, {self(), NewCache}),
    State#state{channel = Channels#{ChannelId => true}}.


format_sub_topic(ProductId, DevAddr, Topics) ->
    format_topic(<<"pub">>, ProductId, DevAddr, Topics).

format_topic(Type0, ProductId, DevAddr, #{<<"topics">> := Topics0}) ->
    Replace = [{<<"\\$\\{ProductId\\}">>, ProductId}, {<<"\\$\\{DevAddr\\}">>, DevAddr}],
    Topics = [
        #{<<"topic">> => <<"thing/${ProductId}/${DevAddr}/post">>, <<"type">> => <<"pub">>},
        #{<<"topic">> => <<"thing/${ProductId}/${DevAddr}">>, <<"type">> => <<"sub">>}
    ] ++ Topics0,
    lists:foldl(
        fun
            (#{<<"type">> := Type, <<"topic">> := Topic}, Acc) ->
                NewTopic =
                    lists:foldl(
                        fun({Re, Rp}, Topic1) ->
                            re:replace(Topic1, Re, Rp, [global, {return, binary}])
                        end, Topic, Replace),
                case Type0 == Type of
                    true ->
                        [NewTopic | Acc];
                    false ->
                        Acc
                end
        end, [], Topics).

format_device(Args) ->
    Args.


do_message(Type, Addr, ProductId, Message) ->
    dgiot_product:do_handler(ProductId, 'message.publish', Message#{
        <<"type">> => Type,
        <<"productId">> => ProductId,
        <<"addr">> => Addr
    }).

%% 设备数据更新到库
save_device(undefined, _) -> ok;
save_device(ObjectId, #{action := Action, basedata := BaseData}) ->
    dgiot_parse_cache:save_to_cache(#{
        pid => self(),
        action => Action,
        <<"method">> => <<"PUT">>,
        <<"path">> => <<"/classes/Device/", ObjectId/binary>>,
        <<"body">> => BaseData#{
            <<"lastOnlineTime">> => dgiot_datetime:nowstamp()
        }
    }).

%% 创建设备
create_device(ProductId, #{action := Action, basedata := BaseData}) ->
    Keys = [
        <<"devaddr">>, <<"product">>,
        <<"ip">>, <<"parentId">>, <<"brand">>,
        <<"name">>, <<"isEnable">>, <<"status">>,
        <<"route">>, <<"ACL">>, <<"name">>,
        <<"devModel">>, <<"location">>, <<"basedata">>
    ],
    NewDevice = maps:with(Keys, BaseData#{
        <<"product">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"Product">>,
            <<"objectId">> => ProductId
        },
        <<"status">> => <<"ONLINE">>,
        <<"lastOnlineTime">> => dgiot_datetime:nowstamp()
    }),
    Data = #{
        pid => self(),
        action => Action,
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/classes/Device">>,
        <<"body">> => NewDevice
    },
    dgiot_parse_cache:save_to_cache(Data).


%% 子设备与网关关系
%% route 字段，网关地址：短地址
%% 例 网关 000001 则子设备000002的route为 {"000001":20}
load_children(DevAddr) ->
    Keys = [<<"devaddr">>, <<"isEnable">>, <<"route">>, <<"product">>],
    Query = #{
        pid => self(),
        action => load_children,
        <<"method">> => <<"GET">>,
        <<"path">> => <<"/classes/Device">>,
        <<"body">> => #{
            <<"keys">> => Keys,
            <<"where">> => #{
                <<"route.", DevAddr/binary>> => #{
                    <<"$regex">> => <<".+">>
                }
            },
            <<"limit">> => 1500,
            <<"include">> => <<"product">>
        }
    },
    dgiot_parse_cache:save_to_cache(Query).


%% 属性上报
check_property(Data, Properties) ->
    check_property(Data, Properties, []).
check_property([], _, Acc) -> Acc;
check_property([{Identifier, Value} | Data], Properties, Acc) ->
    case find_prop(Identifier, Properties) of
        {error, not_find} ->
            check_property(Data, Properties, Acc);
        {ok, Property} ->
            case check_value(Value, Property) of
                false ->
                    check_property(Data, Properties, Acc);
                {true, NewValue} ->
                    check_property(Data, Properties, [{Identifier, NewValue} | Acc])
            end
    end.

find_prop(_Id, []) -> {error, not_find};
find_prop(Id, [#{<<"identifier">> := Id} = Prop | _]) -> {ok, Prop};
find_prop(Id, [_ | Other]) -> find_prop(Id, Other).

check_value(Value, #{<<"dataType">> := #{<<"type">> := Type0, <<"specs">> := Specs}}) ->
    Type = list_to_binary(string:to_upper(binary_to_list(Type0))),
    Is =
        case Type of
            _ when Type == <<"INT">>; Type == <<"DATE">> ->
                is_integer(Value);
            _ when Type == <<"FLOAT">>; Type == <<"DOUBLE">> ->
                is_float(Value);
            <<"BOOL">> ->
                is_boolean(Value);
            <<"STRING">> ->
                is_binary(Value) orelse is_list(Value);
            <<"ENUM">> ->
                lists:member(Value, maps:keys(Specs));
            <<"STRUCT">> ->
                is_map(Value) orelse is_list(Value)
        end,
    case Is andalso check_validate(Value, Specs) of
        true ->
            {true, change_value(Value, Specs)};
        false ->
            false
    end.


check_validate(Value, #{<<"max">> := Max, <<"min">> := Min}) when is_integer(Max), is_integer(Min) ->
    Value =< Max andalso Value >= Min;
check_validate(Value, #{<<"max">> := Max}) when is_integer(Max) ->
    Value =< Max;
check_validate(Value, #{<<"min">> := Min}) when is_integer(Min) ->
    Value >= Min;
check_validate(_, _) ->
    true.

change_value(Value, #{<<"rate">> := Rate} = Map) ->
    change_value(Value * Rate, maps:remove(<<"rate">>, Map));
change_value(Value, #{<<"offset">> := Offset} = Map) ->
    change_value(Value + Offset, maps:remove(<<"offset">>, Map));
change_value(Value, #{<<"degree">> := Degree}) when Degree > 0 ->
    N = math:pow(10, Degree),
    round(Value * N) / N;
change_value(Value, _) ->
    Value.
