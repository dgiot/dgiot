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

-module(dgiot_instruct).
-author("jonhl").
-include("dgiot_task.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    createsub/6,
    create/6,
    create_group/7,
    get_instruct/4,
    get_child_instruct/3
]).

createsub(ProductId, DeviceId, DtuAddr, ACL, Rotation, #{<<"parentDtu">> := ParentDtu}) ->
    lists:map(fun(X) ->
        #{
            <<"route">> := #{DtuAddr := Pn},
            <<"ACL">> := ACL,
            <<"product">> := #{<<"thing">> := Thing}
        } = X,
        NewPn = <<DtuAddr/binary, "/", Pn/binary>>,
        create(ProductId, DeviceId, NewPn, ACL, Rotation, Thing#{<<"parentDtu">> => ParentDtu})
              end, dgiot_device:get_sub_device(DtuAddr)),
    ok.

create(ProductId, DeviceId, Pn, ACL, Rotation, #{<<"properties">> := Props}) ->
    lists:map(fun(X) ->
        case X of
            #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>}} ->
                pass;
            #{<<"dataForm">> := #{<<"strategy">> := <<"主动上报"/utf8>>}} ->
                pass;
            #{<<"accessMode">> := Op, <<"dataForm">> := #{<<"address">> := Di, <<"order">> := Order} = DataForm,
                <<"name">> := Name, <<"identifier">> := Identifier, <<"required">> := Enable} ->
                case Di of
                    <<"">> -> pass;
                    _ ->
                        ObjectId = dgiot_parse:get_instruct(DeviceId, Pn, Di),
                        case dgiot_parse:get_object(<<"Instruct">>, ObjectId) of
                            {ok, _} ->
                                pass;
                            _ ->
                                Map = #{<<"ACL">> => ACL, <<"enable">> => Enable,
                                    <<"product">> => #{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"Product">>,
                                        <<"objectId">> => ProductId
                                    },
                                    <<"device">> => #{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"Device">>,
                                        <<"objectId">> => DeviceId
                                    },
                                    <<"name">> => Name, <<"order">> => dgiot_utils:to_binary(Order),
                                    <<"pn">> => Pn, <<"di">> => Di,
                                    <<"op">> => Op, <<"interval">> => 20,
                                    <<"duration">> => 5, <<"rotation">> => Rotation,
                                    <<"other">> => DataForm#{<<"identifier">> => Identifier}
                                },
                                dgiot_parse:create_object(<<"Instruct">>, Map)
                        end
                end;
            Other ->
                ?LOG(info, "Other ~p", [Other]),
                pass
        end
              end, Props).

create_group(ProductId, DeviceId, Group, Pn, ACL, Rotation, #{<<"properties">> := Props} = Thing) ->
    lists:map(fun(X) ->
        #{
            <<"accessMode">> := Op,
            <<"dataForm">> := #{
                <<"address">> := Di
            },
            <<"name">> := Name,
            <<"identifier">> := Identifier,
            <<"required">> := Enable
        } = X,
        case Di of
            <<"">> -> pass;
            _ -> case dgiot_parse:query_object(<<"Instruct">>, #{<<"where">> => #{
                <<"product">> => ProductId,
                <<"device">> => DeviceId,
                <<"pn">> => Pn,
                <<"di">> => Di}}) of
                     {ok, #{<<"results">> := []}} ->
                         Other = maps:without([<<"properties">>], Thing),
                         Map = #{
                             <<"ACL">> => ACL,
                             <<"enable">> => Enable,
                             <<"product">> => #{
                                 <<"__type">> => <<"Pointer">>,
                                 <<"className">> => <<"Product">>,
                                 <<"objectId">> => ProductId
                             },
                             <<"device">> => #{
                                 <<"__type">> => <<"Pointer">>,
                                 <<"className">> => <<"Device">>,
                                 <<"objectId">> => DeviceId
                             },
                             <<"name">> => Name,
                             <<"order">> => Group,
                             <<"pn">> => Pn,
                             <<"di">> => Di,
                             <<"op">> => Op,
                             <<"interval">> => 30,
                             <<"duration">> => 5,
                             <<"rotation">> => Rotation,
                             <<"other">> => Other#{<<"identifier">> => Identifier}
                         },
                         dgiot_parse:create_object(<<"Instruct">>, Map);
                     _ ->
                         pass
                 end
        end
              end, Props).

init_que(DeviceId, Round) ->
    case dgiot_parse:query_object(<<"Instruct">>, #{<<"order">> => <<"-order">>, <<"where">> => #{<<"device">> => DeviceId}}) of
        {ok, #{<<"results">> := []}} ->
            [];
        {ok, #{<<"results">> := List}} ->
            NewList = lists:foldl(
                fun(X, Acc) ->
                    case X of
                        #{<<"enable">> := true, <<"op">> := Op, <<"order">> := Order, <<"pn">> := Pn, <<"di">> := Di,
                            <<"interval">> := Interval, <<"other">> := DataForm} ->
                            Identifier = maps:get(<<"accessMode">>, DataForm, <<"">>),
                            AccessMode = maps:get(<<"accessMode">>, DataForm, Op),
                            Address = maps:get(<<"address">>, DataForm, Di),
                            Protocol = maps:get(<<"protocol">>, DataForm, <<"">>),
                            ThingRound = maps:get(<<"round">>, DataForm, <<"all">>),
                            InstructOrder = maps:get(<<"order">>, DataForm, Order),
                            Data = maps:get(<<"data">>, DataForm, <<"null">>),
                            Control = maps:get(<<"control">>, DataForm, "%d"),
                            NewData = dgiot_task:get_control(Round, Data, Control),
                            Strategy = dgiot_utils:to_int(maps:get(<<"strategy">>, DataForm, Interval)),
                            case ThingRound of
                                <<"all">> ->
                                    Acc ++ [{Order, {InstructOrder, Strategy, Identifier, Pn, Address, AccessMode, NewData, Protocol, ThingRound}}];
                                Round ->
                                    Acc ++ [{Order, {InstructOrder, Strategy, Identifier, Pn, Address, AccessMode, NewData, Protocol, ThingRound}}];
                                RoundList ->
                                    case lists:member(Round, RoundList) of
                                        true ->
                                            Acc ++ [{Order, {InstructOrder, Strategy, Identifier, Pn, Address, AccessMode, NewData, Protocol, ThingRound}}];
                                        false ->
                                            Acc
                                    end
                            end;
                        _ -> Acc
                    end
                end, [], List),
            lists:foldl(fun(X, Acc1) ->
                {_, Y} = X,
                Acc1 ++ [Y]
                        end, [], lists:keysort(1, NewList));
        _ -> []
    end.

get_instruct(ProductId, _DeviceId, Round, thing) ->
    get_instruct(ProductId, Round);

get_instruct(ProductId, DeviceId, Round, instruct) ->
    get_que(ProductId, DeviceId, Round).

get_instruct(ProductId, Round) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} when length(Props) > 0 ->
            {_, NewList} = lists:foldl(fun(X, Acc) ->
                {Order, List} = Acc,
                case X of
                    #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>}} ->
                        Acc;
                    #{<<"dataForm">> := #{<<"strategy">> := <<"主动上报"/utf8>>}} ->
                        Acc;
                    #{<<"accessMode">> := AccessMode, <<"identifier">> := Identifier,
                        <<"dataForm">> := DataForm, <<"dataSource">> := DataSource} ->
                        Protocol = maps:get(<<"protocol">>, DataForm, <<"">>),
                        ThingRound = maps:get(<<"round">>, DataForm, <<"all">>),
                        InstructOrder = maps:get(<<"order">>, DataForm, Order),
                        Data = maps:get(<<"data">>, DataSource, <<"0">>),
                        Control = maps:get(<<"control">>, DataForm, "%d"),
                        NewData = dgiot_task:get_control(Round, Data, Control),
                        Strategy = dgiot_utils:to_int(maps:get(<<"strategy">>, DataForm, "2")),
                        BinRound = dgiot_utils:to_binary(Round),
                        case ThingRound of
                            <<"all">> ->
                                {Order + 1, List ++ [{InstructOrder, Strategy, Identifier, AccessMode, NewData, Protocol, DataSource, ThingRound}]};
                            BinRound ->
                                {Order + 1, List ++ [{InstructOrder, Strategy, Identifier, AccessMode, NewData, Protocol, DataSource, ThingRound}]};
                            Rounds ->
                                RoundList = binary:split(Rounds, <<",">>, [global]),
                                case lists:member(BinRound, RoundList) of
                                    true ->
                                        {Order + 1, List ++ [{InstructOrder, Strategy, Identifier, AccessMode, NewData, Protocol, DataSource, ThingRound}]};
                                    false ->
                                        Acc
                                end
                        end;
                    _ ->
                        Acc
                end
                                       end, {1, []}, Props),
            lists:keysort(1, NewList);
        _ ->
            []
    end.

get_child_instruct(DeviceId, Round, thing) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"parentId">> => DeviceId}}) of
        {ok, #{<<"results">> := ChildDevices}} ->
            lists:foldl(fun(#{<<"product">> := #{<<"objectId">> := ProductId}}, Acc) ->
                Acc ++ dgiot_instruct:get_instruct(ProductId, DeviceId, Round, thing)
                        end, [], ChildDevices);
        _ ->
            []
    end.

get_que(_ProductId, DeviceId, Round) ->
    case dgiot_data:get({instuct, DeviceId}) of
        not_find ->
            Que = init_que(DeviceId, Round),
            dgiot_data:insert({instuct, DeviceId}, Que),
            Que;
        Que ->
            NewQue = get_que_(Que, Round),
            dgiot_data:insert({instuct, DeviceId}, NewQue),
            NewQue
    end.

get_que_(Que, Round) ->
    lists:foldl(fun(X, Acc) ->
        case X of
            {InstructOrder, Strategy, Identifier, Address, AccessMode, NewData, Protocol, ThingRound} ->
                case ThingRound of
                    <<"all">> ->
                        Acc ++ [{InstructOrder, Strategy, Identifier, Address, AccessMode, NewData, Protocol, ThingRound}];
                    Round ->
                        Acc ++ [{InstructOrder, Strategy, Identifier, Address, AccessMode, NewData, Protocol, ThingRound}];
                    RoundList ->
                        case lists:member(Round, RoundList) of
                            true ->
                                Acc ++ [{InstructOrder, Strategy, Identifier, Address, AccessMode, NewData, Protocol, ThingRound}];
                            false ->
                                Acc
                        end
                end;
            _ ->
                Acc
        end
                end, [], Que).


