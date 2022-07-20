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
]).


properties_report(ProductId, DevAddr, Payload) when is_map(Payload) ->
%%    io:format("~s ~p ProductId ~p, DevAddr ~p, Payload: ~p ~n", [?FILE, ?LINE, ProductId, DevAddr, Payload]),
    NewPload = parse_payload(ProductId, Payload),
    dgiot_task:save_td(ProductId, DevAddr, NewPload, #{});

properties_report(ProductId, DevAddr, Payload) ->
    lists:map(fun
                  ({ChannelId, _Ctype}) ->
                      dgiot_channelx:do_message(ChannelId, {dlink_properties_report, ProductId, DevAddr, Payload});
                  (_) ->
                      pass
              end, dgiot_bridge:get_proctol_channel(ProductId)),
%%    io:format("~s ~p ProductId ~p, DevAddr ~p, Payload: ~p ~n", [?FILE, ?LINE, ProductId, DevAddr, Payload]),
    ok.

firmware_report(ProductId, DevAddr, Payload) when is_map(Payload) ->
%%    io:format("~s ~p ProductId ~p, DevAddr ~p, Payload: ~p ~n", [?FILE, ?LINE, ProductId, DevAddr, Payload]),
    NewPload = parse_payload(ProductId, Payload),
    dgiot_task:save_td(ProductId, DevAddr, NewPload, #{});

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
                        Dis =
                            lists:foldl(fun
                                            (#{<<"key">> := Key}, Acc) ->
                                                Acc ++ [Key];
                                            (_, Acc) ->
                                                Acc
                                        end, [], maps:get(<<"dis">>, DtaSource, [])),
                        maps:fold(fun(PK, PV, Acc1) ->
                            case lists:member(PK, Dis) of
                                true ->
                                    Acc1#{Identifier => PV};
                                _ ->
                                    Acc1#{PK => PV}
                            end
                                  end, Acc, Payload);
                    _ ->
                        Acc
                end
                        end, #{}, Props);
        _Error ->
            Payload
    end.
