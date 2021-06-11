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
-module(dgiot_opc_channel).
-behavior(dgiot_channelx).
-define(TYPE, <<"DGIOTOPC">>).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-record(state, {id, env = #{}}).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"OPC采集通道"/utf8>>
    },
    description => #{
        zh => <<"OPC采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"OPCSEVER">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"Kepware.KEPServerEX.V6"/utf8>>,
        title => #{
            zh => <<"OPC服务器"/utf8>>
        },
        description => #{
            zh => <<"OPC服务器"/utf8>>
        }
    },
    <<"OPCGROUP">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"group"/utf8>>,
        title => #{
            zh => <<"OPC分组"/utf8>>
        },
        description => #{
            zh => <<"OPC分组"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => 1
    }).

%% 通道初始化
init(?TYPE, ChannelId, ChannelArgs) ->
    {ProductId, DeviceId, Devaddr} = get_product(ChannelId),
    State = #state{
        id = ChannelId,
        env = ChannelArgs#{
            <<"productid">> => ProductId,
            <<"deviceid">> => DeviceId,
            <<"devaddr">> => Devaddr}
    },
    {ok, State}.

%% 初始化池子
handle_init(State) ->
    dgiot_mqtt:subscribe(<<"dgiot_opc_da_ack">>),
    dgiot_mqtt:subscribe(<<"dgiot_opc_da_scan">>),
    dgiot_parse:subscribe(<<"Device">>, post),
%%    erlang:send_after(1000 * 5, self(), scan_opc),
%%    erlang:send_after(1000 * 10, self(), send_opc),
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info,"channel ~p, ~p", [EventId, Event]),
    ok.

handle_message({sync_parse, Args}, State) ->
    ?LOG(info,"sync_parse ~p", [Args]),
    {ok, State};

handle_message(scan_opc, #state{env = Env} = State) ->
    dgiot_opc:scan_opc(Env),
    {ok, State};

%% {"cmdtype":"read",  "opcserver": "ControlEase.OPC.2",   "group":"小闭式",  "items": "INSPEC.小闭式台位计测.U_OPC,INSPEC.小闭式台位计测.P_OPC,
%%    INSPEC.小闭式台位计测.I_OPC,INSPEC.小闭式台位计测.DJZS_OPC,INSPEC.小闭式台位计测.SWD_OPC,
%%    INSPEC.小闭式台位计测.DCLL_OPC,INSPEC.小闭式台位计测.JKYL_OPC,INSPEC.小闭式台位计测.CKYL_OPC","noitemid":"000"}
handle_message(send_opc, #state{id = ChannelId, env = Env} = State) ->
    #{<<"OPCSEVER">> := OpcServer, <<"productid">> := ProductId} = Env,
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"thing">> := Thing,<<"name">> := ProductName}} ->
            #{<<"properties">> := Properties} = Thing,
            Identifier = [maps:get(<<"scan_instruct">>, H) || H <- Properties],
            Identifier1 = [binary:bin_to_list(H) || H <- Identifier],
            Instruct = [ X ++ "," || X <- Identifier1],
            Instruct1 = lists:droplast(lists:concat(Instruct)),
            Instruct2 = erlang:list_to_binary(Instruct1),
            dgiot_opc:read_opc(ChannelId, OpcServer, ProductName, Instruct2),
            erlang:send_after(2000 * 10, self(), send_opc);
        _ ->
            handle_message(send_opc, #state{id = ChannelId, env = Env} = State)
    end,
    {ok, State};


%%{"status":0,"小闭式":{"INSPEC.小闭式台位计测.U_OPC":380,"INSPEC.小闭式台位计测.P_OPC":30}}
handle_message({deliver, _Topic, Msg}, #state{id = ChannelId, env = Env} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    #{<<"productid">> := ProductId, <<"deviceid">> := DeviceId, <<"devaddr">> := Devaddr} = Env,
    dgiot_bridge:send_log(ChannelId, "from opc scan: ~p  ", [Payload]),
    case jsx:is_json(Payload) of
        false ->
            pass;
        true ->
            case jsx:decode(Payload, [return_maps]) of
                #{<<"status">> := 0} = Map0 ->
                    [Map1 | _] = maps:values(maps:without([<<"status">>], Map0)),
                    Data = maps:fold(fun(K, V, Acc) ->
                        case binary:split(K, <<$.>>, [global, trim]) of
                            [_, _, Key] ->
                                Acc#{Key => V};
                            _ -> Acc
                        end
                                     end, #{}, Map1),
                    Base64 = get_optshape(ProductId, DeviceId, Data),
                    Url1 = <<"http://127.0.0.1/iotapi/send_topo">>,
                    Data1 = #{<<"productid">> => ProductId, <<"devaddr">> => Devaddr, <<"base64">> => Base64},
                    push(Url1, Data1),
                    case dgiot_data:get({dev, status, DeviceId}) of
                        not_find ->
                            dgiot_data:insert({dev, status, DeviceId}, self()),
                            dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"ONLINE">>});
                        _ -> pass

                    end,
                    dgiot_tdengine_adapter:save(ProductId, Devaddr, Data);
                _ ->
                    pass
            end,
            Map = jsx:decode(Payload, [return_maps]),
            List = maps:to_list(Map),
            D = dict:from_list(List),
            Predicate2=fun(X) ->
                X /= 95 end, % '_' -> 95 Unicode
            Predicate = fun(K,_V) ->
                lists:all(Predicate2,binary:bin_to_list(K)) end,

            D1 = dict:filter(Predicate, D),
            List1=dict:to_list(D1),
            case erlang:length(List1) of
                1 ->
                    {_,Dianwei} = lists:last(List1), %%Dianwei = [{"Name":"Acrel","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.Acrel","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"current","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.current","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"effect","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.effect","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"factor","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.factor","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"flow","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵 模拟.flow","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"head","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.head","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"opening","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.opening","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"power","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.power","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"pressure_in","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.pressure_in","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"pressure_out","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.pressure_out","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"speed","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模 拟.speed","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"switch","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.switch","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"temperature","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.temperature","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"torque","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.torque","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false},{"Name":"vol","HasChildren":false,"IsItem":true,"ItemId":"opc.水泵模拟.vol","ItemProperties":{"ErrorId":{"Failed":false,"Succeeded":true},"Properties":[]},"IsHint":false}]
                    Name_and_item = get_name_and_itemid(Dianwei),
                    Final_Properties = create_final_Properties(Name_and_item),
                    case dgiot_parse:get_object(<<"Product">>, ProductId) of
                        {ok, Result} ->
                            case maps:is_key(<<"thing">>,Result) of
                                false ->
                                    dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"thing">> => #{<<"properties">> => Final_Properties}});
                                true ->
                                    pass
                            end;
                        Error2 -> ?LOG(info,"Error2 ~p ", [Error2])
                    end;
                _ ->
                    pass

            end
    end,
    {ok, State};


handle_message(Message, State) ->
    ?LOG(info,"channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

get_product(ChannelId) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _, [ProductId | _]} ->
            Filter = #{<<"where">> => #{<<"product">> => ProductId}, <<"limit">> => 1},
            case dgiot_parse:query_object(<<"Device">>, Filter) of
                {ok, #{<<"results">> := Results}} when length(Results) == 1 ->
                    [#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr} | _] = Results,
                    {ProductId, DeviceId, Devaddr};
                _ ->
                    {<<>>, <<>>, <<>>}
            end;
        _ ->
            {<<>>, <<>>, <<>>}
    end.


get_optshape(ProductId, DeviceId, Payload) ->
    Shape =
        maps:fold(fun(K, V, Acc) ->
            Text = dgiot_topo:get_name(ProductId, K, dgiot_utils:to_binary(V)),
            Type =
                case dgiot_data:get({shapetype, dgiot_parse:get_shapeid(ProductId, K)}) of
                    not_find ->
                        <<"text">>;
                    Type1 ->
                        Type1
                end,
            Acc ++ [#{<<"id">> => dgiot_parse:get_shapeid(DeviceId, K), <<"text">> => Text, <<"type">> => Type}]
                  end, [], Payload),
    base64:encode(jsx:encode(#{<<"konva">> => Shape})).


push(Url, Data) ->
    Url1 = dgiot_utils:to_list(Url),
    Data1 = dgiot_utils:to_list(jsx:encode(Data)),
    httpc:request(post, {Url1, [], "application/json", Data1}, [], []).


%%创建物模型
create_Properties({Name,Identifier}) ->
    #{<<"accessMode">> => <<"r">>,
        <<"dataForm">> =>
        #{<<"address">> =>
        <<"00000000">>,
            <<"byteorder">> => <<"big">>,
            <<"collection">> => <<"%s">>,
            <<"control">> => <<"%q">>,<<"data">> => <<"null">>,
            <<"offset">> => 0,<<"protocol">> => <<"normal">>,
            <<"quantity">> => <<"null">>,<<"rate">> => 1,
            <<"strategy">> => <<"20">>},
        <<"dataType">> =>
        #{<<"specs">> =>
        #{<<"max">> => 100,<<"min">> => 0,
            <<"step">> => 0.01,<<"unit">> => <<>>},
            <<"type">> => <<"float">>},
        <<"identifier">> => Name,
        <<"name">> => Name,
        <<"scan_instruct">> => Identifier,
        <<"required">> => true}.



create_final_Properties(List) -> [ create_Properties(X) || X <- List].


add_to_list(Map) ->
    #{<<"Name">> := Name,<<"ItemId">> := ItemId } = Map,
    [{Name,ItemId}].

get_name_and_itemid([H|T]) ->
    add_to_list(H) ++ get_name_and_itemid(T);
get_name_and_itemid([]) ->
    [].


%%%创建组态config

