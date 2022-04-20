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
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-record(state, {id, step, env = #{}}).

%% API
-export([start/2]).
-export([init/3, handle_event/3, handle_message/2, handle_init/1, stop/3]).


%% 注册通道类型
-channel_type(#{

    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
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
    },
    <<"Topic">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"dgiot_opc_da"/utf8>>,
        title => #{
            zh => <<"订阅Topic"/utf8>>
        },
        description => #{
            zh => <<"订阅Topic"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/OPC_ICO.png">>,
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
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs#{
        <<"Size">> => 1
    }).

%% 通道初始化
init(?TYPE, ChannelId, ChannelArgs) ->
    {ProductId, Deviceinfo_list} = get_product(ChannelId), %Deviceinfo_list = [{DeviceId1,Devaddr1},{DeviceId2,Devaddr2}...]
    State = #state{
        id = ChannelId,
        env = ChannelArgs#{
            <<"productid">> => ProductId,
            <<"deviceinfo_list">> => Deviceinfo_list
            }
    },
    dgiot_parse_hook:subscribe(<<"Device">>, post, ChannelId),
    {ok, State}.

%% 初始化池子
handle_init(State) ->
    #state{env = #{<<"Topic">> := Topic}} = State,
    Topic_ACK = binary:bin_to_list(Topic) ++ "_ack",
    Topic_SCAN = binary:bin_to_list(Topic) ++ "_scan",
    dgiot_mqtt:subscribe( erlang:list_to_binary(Topic_ACK)),
    dgiot_mqtt:subscribe( erlang:list_to_binary(Topic_SCAN)),
    erlang:send_after(1000 * 5, self(), scan_opc),
    erlang:send_after(1000*60*10,self(),offline_jud),
    {ok, State}.



%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventId, Event]),
    ok.

%%设备下线状态修改
handle_message(offline_jud, #state{env = Env} = State) ->
    erlang:send_after(1000*60*10,self(),offline_jud),
    #{<<"deviceinfo_list">> := Deviceinfo_list} = Env,
    [offline_modify(DeviceID) ||{DeviceID,_} <- Deviceinfo_list],
    {ok, State};

handle_message({sync_parse, _Method, Args}, State) ->
    ?LOG(info,"sync_parse ~p", [Args]),
    {ok, State};

handle_message(scan_opc, #state{env = Env} = State) ->
    dgiot_opc:scan_opc(Env),
%%    #{<<"Topic">> := Topic} = Env,
%%    ?LOG(info,"------------------------Env:~p",[Topic]),
    {ok, State#state{step = scan}};

%% {"cmdtype":"read",  "opcserver": "ControlEase.OPC.2",   "group":"小闭式",  "items": "INSPEC.小闭式台位计测.U_OPC,INSPEC.小闭式台位计测.P_OPC,
%%    INSPEC.小闭式台位计测.I_OPC,INSPEC.小闭式台位计测.DJZS_OPC,INSPEC.小闭式台位计测.SWD_OPC,
%%    INSPEC.小闭式台位计测.DCLL_OPC,INSPEC.小闭式台位计测.JKYL_OPC,INSPEC.小闭式台位计测.CKYL_OPC","noitemid":"000"}
handle_message(read_opc, #state{id = ChannelId, step = read_cycle ,env = #{<<"OPCSEVER">> := OpcServer,<<"Topic">> := Topic, <<"productid">> := ProductId,<<"deviceinfo_list">> := Deviceinfo_list}} = State) ->
    {ok,#{<<"name">> :=ProductName}} = dgiot_parse:get_object(<<"Product">>,ProductId),
    DeviceName_list = [get_DevAddr(X)|| X <- Deviceinfo_list],
    case dgiot_device:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
            Item2 = [maps:get(<<"identifier">>, H) || H <- Properties],
            Identifier_item = [binary:bin_to_list(H) || H <- Item2],
            Instruct = [binary:bin_to_list(ProductName) ++ "." ++ binary:bin_to_list(Y) ++ "." ++ X ++ "," || X <- Identifier_item,Y <- DeviceName_list],
            Instruct1 = lists:droplast(lists:concat(Instruct)),
            Instruct2 = erlang:list_to_binary(Instruct1),
            dgiot_opc:read_opc(ChannelId, OpcServer,Topic,Instruct2);
        _ ->
            pass
    end,
    {ok, State#state{step = read}};

%%{"status":0,"小闭式":{"INSPEC.小闭式台位计测.U_OPC":380,"INSPEC.小闭式台位计测.P_OPC":30}}
handle_message({deliver, _Topic, Msg}, #state{id = ChannelId, step = scan, env = Env} = State) ->
    #{<<"productid">> := ProductId,<<"Topic">> :=Topic} = Env,
    Payload = dgiot_mqtt:get_payload(Msg),
    #{<<"OPCSEVER">> := OpcServer,<<"OPCGROUP">> := Group } = Env,
    dgiot_bridge:send_log(ChannelId, "from opc scan: ~p  ", [Payload]),
    case jsx:is_json(Payload) of
        false ->
            {ok, State};
        true ->
            dgiot_opc:scan_opc_ack(Payload,OpcServer,Topic, Group,ProductId),
            {ok, State#state{step = pre_read}}
    end;

handle_message({deliver, _Topic, Msg}, #state{ step = pre_read, env = Env} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    #{<<"productid">> := ProductId} = Env,
    case jsx:is_json(Payload) of
        false ->
            pass;
        true ->
            case jsx:decode(Payload, [return_maps]) of
                #{<<"status">> := 0} = Map0 ->
                    [Map1 | _] = maps:values(maps:without([<<"status">>], Map0)),
                    case maps:find(<<"status">>,Map1) of
                        {ok,_} ->
                            [Map2 | _] = maps:values(maps:without([<<"status">>], Map1));
                        error ->
                            Map2 = Map1

                    end,
                    Data = maps:fold(fun(K, V, Acc) ->
                        case binary:split(K, <<$.>>, [global, trim]) of
                            [_, _, Key1,Key2] ->
                                Key3 =erlang:list_to_binary(binary:bin_to_list(Key1) ++ binary:bin_to_list(Key2)),
                                Acc#{Key3 => V,Key1 =>Key1 };
                            [_,_,_] ->
                                Acc#{K => K};
                            _ -> Acc
                        end
                                     end, #{}, Map2),
                    List_Data = maps:to_list(Data),
                    Need_update_list0 = dgiot_opc:create_changelist(List_Data),
                    Need_update_list =unique_1(Need_update_list0),
%%                    ?LOG(info,"--------------------Need_update_list:~p",[Need_update_list]),
                    Final_Properties = dgiot_opc:create_final_Properties(Need_update_list),
                    Topo_para=lists:zip(Need_update_list,dgiot_opc:create_x_y(erlang:length(Need_update_list))),
                    New_config = dgiot_opc:create_config(dgiot_opc:change_config(Topo_para)),
                    dgiot_product:load(ProductId),
                    case dgiot_device:lookup_prod(ProductId) of
                        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} ->
                            case erlang:length(Properties)  of
                                0 ->
                                    dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"config">> =>  New_config}),
                                    dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"thing">> => #{<<"properties">> => Final_Properties}});
                                _ ->
                                    pass
                            end
                    end,
                    {ok, State#state{step = read}}
            end
    end;



handle_message({deliver, _Topic, Msg}, #state{id = ChannelId, step = read, env = Env} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    #{<<"productid">> := ProductId, <<"deviceinfo_list">> := Deviceinfo_list} = Env,  %Deviceinfo_list = [{DeviceId1,Devaddr1},{DeviceId2,Devaddr2}...]
    dgiot_bridge:send_log(ChannelId, "from opc read: ~p  ", [jsx:decode(Payload, [return_maps])]),
    case jsx:is_json(Payload) of
        false ->
            pass;
        true ->
            [dgiot_opc:read_opc_ack(Payload, ProductId,X) || X<-Deviceinfo_list],
            erlang:send_after(1000 * 10, self(), read_opc)

    end,
    {ok, State#state{step = read_cycle}};

handle_message(Message, State) ->
    ?LOG(info,"channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(info,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

get_product(ChannelId) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _, [ProductId | _]} ->
            Filter = #{<<"where">> => #{<<"product">> => ProductId},<<"limit">> => 10},
            case dgiot_parse:query_object(<<"Device">>, Filter) of
                {ok, #{<<"results">> := Results}} ->
                    Deviceinfo_list = [get_deviceinfo(X)||X<-Results],
                    {ProductId, Deviceinfo_list};
                _ ->
                    {<<>>, [{<<>>, <<>>}]}
            end;
        _ ->
            {<<>>, [{<<>>, <<>>}]}
    end.

get_deviceinfo(X) ->
    #{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr} = X,
    {DeviceId,Devaddr}.


get_DevAddr({_DeviceId,DevAddr}) ->
    DevAddr.

unique_1(List)->
    unique_1(List, []).


unique_1([H|L], ResultList) ->
    case lists:member(H, ResultList) of
        true -> unique_1(L, ResultList);
        false -> unique_1(L, [H|ResultList])
    end;
unique_1([], ResultList) -> ResultList.

offline_jud(DeviceID) ->
    Url = "http://prod.iotn2n.com/iotapi/device/" ++ dgiot_utils:to_list(DeviceID) ++ "?order=-createdAt&limit=1&skip=0" ,
    AuthHeader = [{"authorization","Basic ZGdpb3RfYWRtaW46ZGdpb3RfYWRtaW4="}],
    {ok,{_,_,Result1}} = httpc:request(get,{[Url],AuthHeader},[],[]),
    timer:sleep(1000*60),
    {ok,{_,_,Result2}} = httpc:request(get,{[Url],AuthHeader},[],[]),
    case Result1 == Result2 of
        true ->
            true;
        false ->
            false
    end.

offline_modify(DeviceID) ->
    case offline_jud(DeviceID) of
        true ->
            dgiot_parse:update_object(<<"Device">>, DeviceID, #{<<"status">> => <<"OFFLINE">>});
        false ->
            pass
    end.
