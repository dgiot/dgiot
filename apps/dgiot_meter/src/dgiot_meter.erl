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

%% @doc dgiot_meter Protocol
-module(dgiot_meter).
-include("dgiot_meter.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([parse_frame/3, to_frame/1]).
-export([search_meter/1, search_meter/4]).
-export([
    create_dtu/3,
    create_dtu/4,
    create_meter/4,
    create_meter4G/3,
    get_sub_device/1
]).

-define(APP, ?MODULE).

%%新设备
create_dtu(mqtt, DtuAddr, ProductId, DTUIP) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl}} ->
            Requests = #{
                <<"devaddr">> => DtuAddr,
                <<"name">> => <<"DTU_", DtuAddr/binary>>,
                <<"ip">> => DTUIP,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => <<"DTU", DtuAddr/binary>>,
                <<"devModel">> => <<"DTU_">>
            },
            dgiot_device:create_device(Requests);
        _ ->
            pass
    end.

create_dtu(DtuAddr, ChannelId, DTUIP) ->
    case dgiot_data:get({dtu, ChannelId}) of
        {ProductId, Acl, _Properties} ->
            Requests = #{
                <<"devaddr">> => DtuAddr,
                <<"name">> => <<"DTU_", DtuAddr/binary>>,
                <<"ip">> => DTUIP,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => <<"DTU", DtuAddr/binary>>,
                <<"devModel">> => <<"DTU_">>
            },
            dgiot_device:create_device(Requests);
        _ ->
            pass
    end.


create_meter(MeterAddr, ChannelId, DTUIP, DtuAddr) ->
    case dgiot_data:get({meter, ChannelId}) of
        {ProductId, ACL, _Properties} ->
            Requests = #{
                <<"devaddr">> => MeterAddr,
                <<"name">> => <<"Meter_", MeterAddr/binary>>,
                <<"ip">> => DTUIP,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => ACL,
                <<"route">> => #{DtuAddr => MeterAddr},
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => <<"Meter", MeterAddr/binary>>,
                <<"devModel">> => <<"Meter">>
            },
            dgiot_device:create_device(Requests),
            {DtuProductId, _, _} = dgiot_data:get({dtu, ChannelId}),
            dgiot_task:save_pnque(DtuProductId, DtuAddr, ProductId, MeterAddr);
        _ ->
            pass
    end.


create_meter4G(DevAddr, ChannelId, DTUIP) ->
    case dgiot_data:get({dtu, ChannelId}) of
        {ProductId, ACL, _Properties} ->
            Requests = #{
                <<"devaddr">> => DevAddr,
                <<"name">> => <<"Concentrator_", DevAddr/binary>>,
                <<"ip">> => DTUIP,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => ACL,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => <<"Concentrator", DevAddr/binary>>,
                <<"devModel">> => <<"Concentrator">>
            },
            dgiot_device:create_device(Requests);
%%            dgiot_task:save_pnque(ProductId, DevAddr, ProductId, DevAddr);
        _ ->
            pass
    end.


get_sub_device(DtuAddr) ->
    Query = #{<<"keys">> => [<<"devaddr">>, <<"product">>,<<"route">>],
        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 256},
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

parse_frame(?DLT645, Buff, Opts) ->
    {Rest, Frames} = dlt645_decoder:parse_frame(Buff, Opts),
    {Rest, lists:foldl(fun(X, Acc) ->
        Acc ++ [maps:without([<<"diff">>, <<"send_di">>], X)]
                       end, [], Frames)};

parse_frame(?DLT376, Buff, Opts) ->
    {Rest, Frames} = dlt376_decoder:parse_frame(Buff, Opts),
    {Rest, lists:foldl(fun(X, Acc) ->
        Acc ++ [maps:without([<<"diff">>, <<"send_di">>], X)]
                       end, [], Frames)}.

% DLT376发送抄数指令
to_frame(#{
    <<"devaddr">> := Addr,
    <<"di">> := Di,
    <<"command">> := <<"r">>,
    <<"protocol">> := ?DLT376,
    <<"data">> := <<"null">>
} = Frame) ->
    dlt376_decoder:to_frame(Frame#{
        <<"msgtype">> => ?DLT376,
        <<"addr">> => dlt376_proctol:decode_of_addr(dgiot_utils:hex_to_binary(Addr)),
        <<"data">> => <<>>,
        <<"di">> => Di,
        <<"command">> => ?DLT376_MS_READ_DATA,
        <<"afn">> => ?DLT376_MS_READ_DATA_AFN
    });

% DLT645 组装电表抄表指令
to_frame(#{
    <<"devaddr">> := Addr,
    <<"di">> := Di,
    <<"command">> := <<"r">>,
    <<"protocol">> := ?DLT645,
    <<"data">> := <<"null">>
} = Frame) ->
    dlt645_decoder:to_frame(Frame#{
        <<"msgtype">> => ?DLT645,
        <<"addr">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Addr)),
        <<"di">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Di)),
        <<"data">> => <<>>,
        <<"command">> => ?DLT645_MS_READ_DATA
    });


% DLT645 组装电表控制指令
to_frame(#{
    <<"devaddr">> := Addr,
    <<"ctrlflag">> := CtrlFlag,
    <<"protocol">> := ?DLT645,
    <<"devpass">> := DevPass,
    <<"apiname">> := get_meter_ctrl
} = Frame) ->
    case CtrlFlag of
        true ->
            PassGrade = <<"02">>,
            Di = <<(dgiot_utils:hex_to_binary(DevPass))/binary, (dgiot_utils:hex_to_binary(PassGrade))/binary>>,
            Data = <<"111111111C00010101010133">>,
            dlt645_decoder:to_frame(Frame#{
                <<"msgtype">> => ?DLT645,
                <<"addr">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Addr)),
                <<"di">> => dlt645_proctol:reverse(Di),
                <<"data">> => dgiot_utils:hex_to_binary(Data),
                <<"command">> => ?DLT645_MS_FORCE_EVENT
            });
        false ->
            PassGrade = <<"02">>,
            Di = <<(dgiot_utils:hex_to_binary(DevPass))/binary, (dgiot_utils:hex_to_binary(PassGrade))/binary>>,
            Data = <<"111111111A00010101010133">>,
            dlt645_decoder:to_frame(Frame#{
                <<"msgtype">> => ?DLT645,
                <<"addr">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Addr)),
                <<"data">> => dgiot_utils:hex_to_binary(Data),
                <<"di">> => dlt645_proctol:reverse(Di),
                <<"command">> => ?DLT645_MS_FORCE_EVENT
            })
    end;

% DLT376 远程电表控制（透明转发）
to_frame(#{
    <<"devaddr">> := DevAddr,
    <<"ctrlflag">> := CtrlFlag,
    <<"devpass">> := DevPass,
    <<"protocol">> := ?DLT376,
    <<"apiname">> := get_meter_ctrl
} = Frame) ->
    Di = <<"00000100">>,
    Data = <<16#02, 16#6B, 16#64, 16#64, 16#1C, 16#00>>,
    Data2 = <<16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>,
    CtrlPayload = to_frame(Frame#{
        <<"protocol">> => ?DLT645,
        <<"devaddr">> => DevAddr,
        <<"ctrlflag">> => CtrlFlag,
        <<"devpass">> => DevPass,
        <<"apiname">> => get_meter_ctrl
    }),
    DataNew = <<Data/binary, CtrlPayload/binary, Data2/binary>>,
    % ?LOG(info, "GGM 230 to_frame, DataNew ~p~n~n~n",[dgiot_utils:binary_to_hex(DataNew)]),
    RetPlayload = dlt376_decoder:to_frame(Frame#{
        <<"msgtype">> => ?DLT376,
        <<"addr">> => dlt376_proctol:decode_of_addr(dgiot_utils:hex_to_binary(DevAddr)),
        <<"data">> => dgiot_utils:binary_to_hex(DataNew),
        <<"di">> => Di,
        <<"command">> => ?DLT376_MS_READ_DATA,
        <<"afn">> => ?DLT376_MS_CONVERT_SEND_AFN
    }),
    % ?LOG(info, "GGM 231 to_frame, Payload1 ~p~n~n~n",[dgiot_utils:binary_to_hex(RetPlayload)]),
    RetPlayload;

% DLT645 组装电表获取上次拉闸合闸的时间
to_frame(#{
    <<"devaddr">> := DevAddr,
    <<"ctrlflag">> := CtrlFlag,
    <<"protocol">> := ?DLT645,
    <<"apiname">> := get_meter_ctrl_status
} = Frame) ->
    case CtrlFlag of
        true ->
            Di = <<"1E000101">>,
            dlt645_decoder:to_frame(Frame#{
                <<"msgtype">> => ?DLT645,
                <<"addr">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(DevAddr)),
                <<"di">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Di)),
                <<"data">> => <<>>,
                <<"command">> => ?DLT645_MS_READ_DATA
            });
        false ->
            Di = <<"1D000101">>,
            dlt645_decoder:to_frame(Frame#{
                <<"msgtype">> => ?DLT645,
                <<"addr">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(DevAddr)),
                <<"di">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Di)),
                <<"data">> => <<>>,
                <<"command">> => ?DLT645_MS_READ_DATA
            })
    end;

% DLT376 组装电表获取上次拉闸合闸的时间（透明转发）
to_frame(#{
    <<"devaddr">> := DevAddr,
    <<"ctrlflag">> := CtrlFlag,
    <<"protocol">> := ?DLT376,
    <<"apiname">> := get_meter_ctrl_status
} = Frame) ->
    Di = <<"00000100">>,
    Data = <<16#02, 16#6B, 16#64, 16#64, 16#10, 16#00>>,
    Data2 = <<16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>,
    CtrlPayload = to_frame(Frame#{
        <<"devaddr">> := DevAddr,
        <<"ctrlflag">> := CtrlFlag,
        <<"protocol">> := ?DLT645,
        <<"apiname">> := get_meter_ctrl_status
    }),
    DataNew = <<Data/binary, CtrlPayload/binary, Data2/binary>>,
    % ?LOG(info, "GGM 260 to_frame, DataNew ~p,~n~n~n",[dgiot_utils:binary_to_hex(CtrlPayload)]),
    RetPlayload = dlt376_decoder:to_frame(Frame#{
        <<"msgtype">> => ?DLT376,
        <<"addr">> => dlt376_proctol:decode_of_addr(dgiot_utils:hex_to_binary(DevAddr)),
        <<"data">> => dgiot_utils:binary_to_hex(DataNew),
        <<"di">> => Di,
        <<"command">> => ?DLT376_MS_READ_DATA,
        <<"afn">> => ?DLT376_MS_CONVERT_SEND_AFN
    }),
    RetPlayload;

to_frame(#{
    <<"devaddr">> := Addr,
    <<"di">> := Di,
    <<"protocol">> := ?DLT645,
    <<"command">> := <<"r">>
} = Frame) ->
    dlt645_decoder:to_frame(Frame#{
        <<"msgtype">> => ?DLT645,
        <<"addr">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Addr)),
        <<"di">> => dlt645_proctol:reverse(dgiot_utils:hex_to_binary(Di)),
        <<"command">> => ?DLT645_MS_READ_DATA
    }).

search_meter(tcp, _Ref, TCPState, 0) ->
    Payload = dlt645_decoder:to_frame(#{
        <<"msgtype">> => ?DLT645,
        <<"addr">> => dlt645_proctol:reverse(<<16#AA, 16#AA, 16#AA, 16#AA, 16#AA, 16#AA>>),
        <<"command">> => ?DLT645_MS_READ_DATA,
        <<"di">> => dlt645_proctol:reverse(<<0, 0, 0, 0>>)  %%组合有功
    }),
    ?LOG(info, "Payload ~p", [dgiot_utils:binary_to_hex(Payload)]),
    dgiot_tcp_server:send(TCPState, Payload),
    read_meter;

search_meter(tcp, Ref, TCPState, 1) ->
    case Ref of
        undefined ->
            pass;
        _ ->
            erlang:cancel_timer(Ref)
    end,
    case search_meter(1) of
        <<"finish">> ->
            {undefined, read_meter, <<>>};
        <<"skip">> ->
            {erlang:send_after(1500, self(), search_meter), search_meter, <<>>};
        Payload ->
            dgiot_tcp_server:send(TCPState, Payload),
            {erlang:send_after(1500, self(), search_meter), search_meter, Payload}
    end.

search_meter(1) ->
    Flag =
        case get({search_meter, self()}) of
            undefined ->
                put({search_meter, self()}, 254),
                255;
            16#AA ->
                put({search_meter, self()}, 16#A9),
                <<"skip">>;
            Len when Len < 0 ->
                <<"finish">>;
            Len ->
                put({search_meter, self()}, Len - 1),
                Len
        end,
    case Flag of
        <<"finish">> ->
            <<"finish">>;
        <<"skip">> ->
            <<"skip">>;
        _ ->
            dlt645_decoder:to_frame(#{
                <<"msgtype">> => ?DLT645,
                <<"addr">> => <<Flag:8, 16#AA, 16#AA, 16#AA, 16#AA, 16#AA>>,
                <<"command">> => ?DLT645_MS_READ_DATA,
                <<"di">> => dlt645_proctol:reverse(<<0, 0, 0, 0>>)})   %%组合有功
    end;

search_meter(_) ->
    <<"finish">>.
