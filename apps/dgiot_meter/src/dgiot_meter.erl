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
    create_meter/4,
    get_sub_device/1
]).

-define(APP, ?MODULE).

%%新设备
create_dtu(DtuAddr, ChannelId, DTUIP) ->
    ?LOG(info, "~p", [dgiot_data:get({dtu, ChannelId})]),
    {ProductId, Acl, _Properties} = dgiot_data:get({dtu, ChannelId}),
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
    dgiot_device:create_device(Requests).

create_meter(MeterAddr, ChannelId, DTUIP, DtuAddr) ->
    {ProductId, ACL, _Properties} = dgiot_data:get({meter, ChannelId}),
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
    dgiot_task:save_pnque(DtuProductId, DtuAddr, ProductId, MeterAddr).

get_sub_device(DtuAddr) ->
    Query = #{<<"keys">> => [<<"devaddr">>, <<"product">>],
        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 256},
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

parse_frame(dlt645, Buff, Opts) ->
    {Rest, Frames} = dlt645_decoder:parse_frame(Buff, Opts),
    {Rest, lists:foldl(fun(X, Acc) ->
        Acc ++ [maps:without([<<"diff">>, <<"send_di">>], X)]
                       end, [], Frames)}.

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
        <<"di">> => dlt645_proctol:reverse(<<0, 0, 0, 0>>)
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
                <<"di">> => dlt645_proctol:reverse(<<0, 0, 0, 0>>)})
    end;

search_meter(_) ->
    <<"finish">>.
