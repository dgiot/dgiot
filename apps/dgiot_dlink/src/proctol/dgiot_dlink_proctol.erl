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
%% https://doc.oschina.net/grpc
%% https://www.grpc.io/docs/

-module(dgiot_dlink_proctol).

-include_lib("dgiot/include/logger.hrl").

-export([login/3]).
-export([
    properties_report/3
    , firmware_report/3
    , parse_payload/2
    , nested_map/2
]).

-define(TYPE, <<"DLINK">>).

%% 注册协议类型
-protocol_type(#{
    cType => ?TYPE,
    type => <<"DLINK">>,
    colum => 3,
    title => #{
        zh => <<"DLINK协议"/utf8>>
    },
    description => #{
        zh => <<"DLINK协议"/utf8>>
    }
}).
%% 注册协议参数
-params (#{
    <<"dis">> => #{
        order => 1,
        type => object,
        allowCreate => true,
        required => true,
        default => [
            #{<<"value">> => <<"lable">>, <<"name">> => <<"key">>}
        ],
        title => #{
            zh => <<"数据标识"/utf8>>
        },
        description => #{
            zh => <<"数据标识"/utf8>>
        },
        <<"table">> => [
            #{
                key => <<"key">>,
                order => 2,
                type => string,
                required => true,
                default => <<"data.[0].number"/utf8>>,
                title => #{
                    zh => <<"数据标识"/utf8>>
                },
                description => #{
                    zh => <<"数据标识"/utf8>>
                }
            },
            #{
                key => <<"data">>,
                order => 2,
                type => integer,
                required => true,
                default => <<"2"/utf8>>,
                title => #{
                    zh => <<"数据长度(字节)"/utf8>>
                },
                description => #{
                    zh => <<"数据长度(字节)"/utf8>>
                }
            }
        ]
    }
}).

%% json格式报文
properties_report(ProductId, DevAddr, Payload) when is_map(Payload) ->
%%    io:format("~s ~p ProductId ~p, DevAddr ~p, Payload = ~p.~n", [?FILE, ?LINE, ProductId, DevAddr, Payload]),
    OldPayload =
        case dgiot_hook:run_hook({dlink_properties_report, ProductId}, {DevAddr, Payload}) of
            {ok, [Payload1]} ->
                Payload1;
            _ ->
                Payload
        end,
    NewPload = parse_payload(ProductId, OldPayload),
    dgiot_device_profile:publish(ProductId, DevAddr, NewPload),
    dgiot_device:save_log(ProductId, DevAddr, NewPload, <<"reportProperty">>),
    dgiot_task:save_td(ProductId, DevAddr, NewPload, #{});

%% 二进制报文
properties_report(ProductId, DevAddr, Payload) ->
    lists:map(fun
                  ({ChannelId, _Ctype}) ->
                      dgiot_channelx:do_message(ChannelId, {dlink_properties_report, ProductId, DevAddr, Payload});
                  (_) ->
                      pass
              end, dgiot_bridge:get_proctol_channel(ProductId)),
%%    io:format("~s ~p ProductId ~p, DevAddr ~p, Payload: ~p ~n", [?FILE, ?LINE, ProductId, DevAddr, Payload]),
    ok.

%% json格式报文
firmware_report(ProductId, DevAddr, Payload) when is_map(Payload) ->
    lists:map(fun
                  ({ChannelId, _Ctype}) ->
                      dgiot_channelx:do_message(ChannelId, {dlink_firmware_report, ProductId, DevAddr, Payload});
                  (_) ->
                      pass
              end, dgiot_bridge:get_proctol_channel(ProductId));

%% 二进制报文
firmware_report(ProductId, DevAddr, Payload) ->
    lists:map(fun
                  ({ChannelId, _Ctype}) ->
                      dgiot_channelx:do_message(ChannelId, {dlink_firmware_report, ProductId, DevAddr, Payload});
                  (_) ->
                      pass
              end, dgiot_bridge:get_proctol_channel(ProductId)),
%%    io:format("~s ~p ProductId ~p, DevAddr ~p, Payload: ~p ~n", [?FILE, ?LINE, ProductId, DevAddr, Payload]),
    ok.

login(_A, _B, _C) ->
    ok.

parse_payload(ProductId, Payload) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier,
                        <<"dataSource">> := DtaSource} ->
                        lists:foldl(
                            fun
                                (#{<<"key">> := Key}, Acc1) ->
                                    case dgiot_dlink_proctol:nested_map(Key, Payload) of
                                        {ok, V} ->
                                            Acc1#{Identifier => V};
                                        _ ->
                                            Acc1
                                    end;
                                (_, Acc1) ->
                                    Acc1
                            end, Acc, maps:get(<<"dis">>, DtaSource, []));
                    _ ->
                        Acc
                end
                        end, Payload, Props);
        _Error ->
            Payload
    end.

nested_map(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, V} ->
            {ok, V};
        _ ->
            nested_map(Key, maps:keys(Map), Map)
    end.
nested_map(_, [], _) ->
    undefined;
nested_map(Key, [H | T], Map) ->
    case maps:get(H, Map) of
        HMap when is_map(HMap) ->
            case nested_map(Key, HMap) of
                undefined ->
                    nested_map(Key, T, Map);
                Value ->
                    Value
            end;
        HList when is_list(HList) ->
            nested_list(Key, HList);
        _ ->
            nested_map(Key, T, Map)
    end.

nested_list(Key, List) ->
    nested_list(Key, List, undefined).

nested_list(_, [], Value) ->
    Value;

nested_list(Key, [H | T], Value) when is_map(H) ->
    NewValue =
        case Value of
            undefined ->
                nested_map(Key, H);
            _ ->
                Value
        end,
    nested_list(Key, T, NewValue);

nested_list(Key, [_ | T], Value) ->
    nested_list(Key, T, Value).









