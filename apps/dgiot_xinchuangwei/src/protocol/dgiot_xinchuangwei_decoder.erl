%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2021 12:00
%%%-------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(dgiot_xinchuangwei_decoder).
-author("stoneliu").
-include_lib("dgiot/include/logger.hrl").

%%-record(rtu_req, {slaveId, funcode, address, quality}).
-include("dgiot_xinchuangwei.hrl").
%%
-export([parse_frame/2, crc16/1, to_status_frame/1, set_params/2]).

%设置参数 返回
parse_frame(<<16#AA, 16#11>>, _State) ->
    {<<>>, <<>>};

%读取参数  字典
parse_frame(<<16#AA, 16#12, Addr:8, Len:8, Rest/binary>>, #state{dtuproduct = ProductId, devaddr = DevAddr}) ->
    case Rest of
        <<UserZone:Len/bytes, Crc:1/binary, _Rest1/binary>> ->
            CheckBuf = <<16#AA, 16#12, Addr:8, Len:8, UserZone/binary>>,
            CheckCrc = crc16(CheckBuf),
            case <<CheckCrc>> =:= Crc of
                true ->
                    #{<<"objectId">> := DeviceId} =
                        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
                    case dgiot_parse:get_object(<<"Product">>, ProductId) of
                        {ok, #{<<"config">> := #{<<"basedate">> := #{<<"params">> := Params}}}} ->
                            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                                {ok, #{<<"basedata">> := OldBasedata}} ->
                                    NewBasedata =
                                        lists:foldl(fun(X, Acc) ->
                                            case X of
                                                #{<<"identifier">> := Identifier,
                                                    <<"protocol">> := <<"xinchuangwei">>,
                                                    <<"address">> := <<"0">>,
                                                    <<"type">> := Type,
                                                    <<"bytes">> := Size,
                                                    <<"unit">> := _Unit,
                                                    <<"struct">> := Struct,
                                                    <<"collection">> := Collection} ->
                                                    NewSize = dgiot_utils:to_int(Size) * 8,
                                                    <<Value:NewSize, _/binary>> = UserZone,
                                                    Newvalue = check_field(Value, #{<<"type">> => Type, <<"struct">> => Struct}, <<"计算值"/utf8>>, Collection),
                                                    Acc#{Identifier => Newvalue};
                                                #{<<"identifier">> := Identifier,
                                                    <<"protocol">> := <<"xinchuangwei">>,
                                                    <<"address">> := Address,
                                                    <<"type">> := Type,
                                                    <<"bytes">> := Size,
                                                    <<"unit">> := _Unit,
                                                    <<"struct">> := Struct,
                                                    <<"collection">> := Collection} ->
                                                    NewAddress = dgiot_utils:to_int(Address),
                                                    NewSize = dgiot_utils:to_int(Size) * 8,
                                                    <<_Befovalue:NewAddress/bytes, Value:NewSize, _/binary>> = UserZone,
                                                    Newvalue = check_field(Value, #{<<"type">> => Type, <<"struct">> => Struct}, <<"计算值"/utf8>>, Collection),
                                                    Acc#{Identifier => Newvalue};
                                                _ ->
                                                    Acc
                                            end
                                                    end, #{}, Params),
                                    ?LOG(info, "OldBasedata: ~p", [OldBasedata]),
                                    ?LOG(info, "NewBasedata: ~p", [NewBasedata]),
                                    Basedata = maps:merge(OldBasedata, NewBasedata),
                                    ?LOG(info, "Basedata: ~p", [Basedata]),
                                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"ONLINE">>, <<"basedata">> => Basedata}),
                                    {{params, Basedata}};
                                _ ->
                                    ?LOG(info, "NoDevice: ~p", [DeviceId]),
                                    {<<>>, <<>>}
                            end;
                        _ ->
                            ?LOG(info, "NoProduct: ~p", [ProductId]),
                            {<<>>, <<>>}
                    end;
                false ->
                    ?LOG(info, "CheckCrc ~p != Crc: ~p", [CheckCrc, Crc]),
                    {<<>>, <<>>}
            end;
        _ ->
            ?LOG(info, "Rest ~p", [Rest]),
            {<<>>, <<>>}
    end;

%%读取状态 物模型
parse_frame(<<16#AA, 16#13, Addr:8, Len:8, Rest/binary>>, #state{dtuproduct = ProductId}) ->
    case Rest of
        <<UserZone:Len/bytes, Crc:1/binary, _Rest1/binary>> ->
            CheckBuf = <<16#AA, 16#13, Addr:8, Len:8, UserZone/binary>>,
            CheckCrc = crc16(CheckBuf),
            case <<CheckCrc>> =:= Crc of
                true ->
                    case dgiot_device:lookup_prod(ProductId) of
                        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                            Ack = lists:foldl(fun(X, Acc) ->
                                case X of
                                    #{<<"identifier">> := Identifier,
                                        <<"dataForm">> := #{
                                            <<"protocol">> := <<"xinchuangwei">>,
                                            <<"address">> := <<"0">>,
                                            <<"data">> := Size}} ->
                                        NewSize = dgiot_utils:to_int(Size) * 8,
                                        <<Value:NewSize, _/binary>> = UserZone,
                                        Acc#{Identifier => Value};
                                    #{<<"identifier">> := Identifier,
                                        <<"dataForm">> := #{
                                            <<"protocol">> := <<"xinchuangwei">>,
                                            <<"address">> := Address,
                                            <<"data">> := Size}} ->
                                        NewAddress = dgiot_utils:to_int(Address),
                                        NewSize = dgiot_utils:to_int(Size) * 8,
                                        <<_Befovalue:NewAddress/bytes, Value:NewSize, _/binary>> = UserZone,
                                        Acc#{Identifier => Value};
                                    _Other ->
%%                                        ?LOG(info, "_Other ~p", [_Other]),
                                        Acc
                                end
                                              end, #{}, Props),
                            {status, Ack};
                        _ ->
                            ?LOG(info, "NoProduct: ~p", [ProductId]),
                            {<<>>, <<>>}
                    end;
                false ->
                    ?LOG(info, "CheckCrc ~p != Crc: ~p", [CheckCrc, Crc]),
                    {<<>>, <<>>}
            end;
        _ ->
            ?LOG(info, "Addr ~p", [Addr]),
            ?LOG(info, "Len ~p", [Len]),
            ?LOG(info, "Rest ~p", [Rest]),
            {<<>>, <<>>}
    end;

parse_frame(Buff, _State) ->
    ?LOG(info, "Buff ~p", [Buff]),
    {error, <<>>}.

set_params(Basedata, ProductId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"config">> := #{<<"basedate">> := #{<<"params">> := Params}}}} ->
            Payloads =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"identifier">> := Identifier,
                            <<"protocol">> := <<"xinchuangwei">>,
                            <<"address">> := <<"0">>,
                            <<"bytes">> := 1,
                            <<"setting">> := <<"%s">>} ->
                            case maps:find(Identifier, Basedata) of
                                error ->
                                    Acc;
                                {ok, Value} when erlang:byte_size(Value) == 0 ->
                                    Acc;
                                {ok, Value} ->
                                    Value1 = dgiot_utils:to_int(Value),
                                    Message = <<16#55, 16#01, 16#00, 16#01, Value1>>,
                                    Checksum = crc16(Message),
                                    Payload = <<Message/binary, Checksum>>,
                                    Acc ++ [Payload];
                                _ ->
                                    Acc
                            end;
                        _ ->
%%                            ?LOG(info,"X ~p", [X]),
                            Acc
                    end
                            end, [], Params),
%%            Topic = <<"setParams/", ProductId/binary, "/", Devaddr/binary>>,
            Payloads;
        _ ->
            ?LOG(info, "NoProduct: ~p", [ProductId]),
            pass
    end.

%% 发送读取状态物模型命令
to_status_frame(ProductId) ->
    lists:foldl(fun({Address, Quality} = R, Acc) ->
        ?LOG(info, "R ~p", [R]),
        <<H:8>> = dgiot_utils:hex_to_binary(Address),
        Quality1 = dgiot_utils:to_int(Quality),
        Message = <<16#55, 16#03, H, Quality1>>,
        Checksum = crc16(Message),
        Payload = <<Message/binary, Checksum>>,
        ?LOG(info, "status Payload ~p", [Payload]),
        Acc ++ [Payload]
                end, [], status_encoder(ProductId)).

status_encoder(ProductId) ->
    case dgiot_device:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"dataForm">> := #{<<"address">> := Address,
                        <<"protocol">> := <<"xinchuangwei">>,
                        <<"data">> := Quantity}} ->
                        Acc ++ [{Address, Quantity}];
                    _ ->
                        Acc
                end
                        end, [], Props);
        _ ->
            []
    end.

%% crc校验 16进制和
%% 55030102 => 5b
%% AA1301020102 => c3
%% Check = <<CheckBuf/binary, CheckCrc/binary>>.
%% <<16#55,16#02,16#36,16#00>>
crc16(CheckBuf) when is_binary(CheckBuf) ->
    Z = lists:foldl(fun(X, Acc) ->
        Acc + X
                    end, 0, [X || <<X:8>> <= CheckBuf]),
    <<_:24, Crc:8>> = <<Z:32>>,
    Crc.

check_field(Value, #{<<"type">> := Type, <<"specs">> := _Specs}, <<"20">>, <<"%s">>) ->
    Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
    case Type1 of
        <<"ENUM">> ->
            dgiot_utils:to_binary(Value);
        _ ->
            Value
    end;

check_field(Value, #{<<"type">> := Type, <<"struct">> := _Struct}, _Stage, <<"%s">>) ->
    Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
    case Type1 of
        <<"ENUM">> ->
            dgiot_utils:to_binary(Value);
        _ ->
            Value
    end;

check_field(Value, _DataType, <<"计算值"/utf8>>, Collection) ->
    Str = re:replace(Collection, "%s", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
    NewValue = dgiot_task:string2value(Str),
    dgiot_utils:to_float(NewValue / 1, 3);

check_field(Value, DataType, Strategy, Collection) ->
    ?LOG(info, "Value ~p", [Value]),
    ?LOG(info, "DataType ~p", [DataType]),
    ?LOG(info, "strategy ~p", [Strategy]),
    ?LOG(info, "Collection ~p", [Collection]),
    Value.

