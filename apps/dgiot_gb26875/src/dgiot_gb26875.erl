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
-module(dgiot_gb26875).
-author("stoneliu").
-include_lib("dgiot_gb26875.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([parse_frame/2]).


%% Buff = dgiot_utils:hex_to_binary(<<"3D302E30303030203D302E303030303530303138200D0A">>).
%% Buff = dgiot_utils:hex_to_binary(<<"3D302E303230302D3D302E303130303530303138200D0A">>).
%% Buff = dgiot_utils:hex_to_binary(<<"3D302E30323030203D302E303130303530303138200D0A">>).
%% Buff = dgiot_utils:hex_to_binary(<<"3d31312e303230203d30302e3031303530303138200D0A">>).
%% Buff = <<"=0.0000 =0.000050018 \r\n">>.
%% <<"=", Suttle:6/binary, " ", "=", Tare:5/binary, Y2:1/binary, F1:1/binary, K1:1/binary, K2:1/binary, B1:1/binary, B2:1/binary, C1:1/binary, CR:1/binary, LF:1/binary>> = Buff.
parse_frame(<<"=", Suttle:6/binary, Y1:1/binary, "=", Tare:5/binary, _:9/binary>>, #state{product = ProductId}) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            Ack = lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier,
                        <<"dataForm">> := #{
                            <<"protocol">> := <<"normal">>,
                            <<"address">> := <<"0X01">>,
                            <<"data">> := 6}} ->
                        RelSuttle =
                            case Y1 of
                                <<" ">> ->
                                    dgiot_utils:to_float(lists:reverse(dgiot_utils:to_list(Suttle)));
                                <<"-">> ->
                                    dgiot_utils:to_float(lists:reverse(dgiot_utils:to_list(<<Suttle/binary, "-">>)));
                                _O ->
                                    ?LOG(info, "_O ~p", [_O]),
                                    <<"0">>
                            end,
                        Acc#{Identifier => RelSuttle};
                    #{<<"identifier">> := Identifier,
                        <<"dataForm">> := #{
                            <<"protocol">> := <<"normal">>,
                            <<"address">> := <<"0X02">>,
                            <<"data">> := 5}} ->
                        RelTare = dgiot_utils:to_float(lists:reverse(dgiot_utils:to_list(Tare))),
                        Acc#{Identifier => RelTare};
                    _Other ->
                        ?LOG(info, "_Other ~p", [_Other]),
                        Acc
                end
                              end, #{}, Props),
            {params, Ack};
        _ ->
            {erroe, #{}}
    end;

parse_frame(_Buff, _State) ->
    {error, <<>>}.



