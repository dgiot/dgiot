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

-module(dgiot_group_utils).
-include("dgiot_group.hrl").
-include_lib("dgiot/include/logger.hrl").

%%-compile(export_all).
%%%%%%%%% 时间域处理函数集合%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%get_pns_bits(VcAddr) ->
%%    Result = dgiot_parse:query_object(<<"Smartmeter">>,
%%        #{<<"where">> => #{<<"$and">> =>
%%        [#{<<"pn">> => #{<<"$ne">> => 0},
%%            <<"vcaddr">> => #{<<"$eq">> => VcAddr}
%%        }]}
%%            , <<"limit">> => 5000}),
%%    NewPns = case Result of
%%                 {ok, #{<<"results">> := Meters}} ->
%%                      lists:foldl(
%%                             fun(X, Pns) ->
%%                                 dgiot_utils:set_binary_bits_n(Pns, maps:get(<<"pn">>, X, 0), 1)
%%                             end, <<0:2048>>, Meters);
%%                 {error, _Reason} ->
%%                     <<0:8>>
%%             end,
%%    dgiot_utils:binary_bits_zip(NewPns).

%%get_task_bits_null(VcAddr, Size) ->
%%    Pns = get_pns_bits(VcAddr),
%%    Len = erlang:length(binary_to_list(Pns)) * Size,
%%    <<0:Len>>.
%%
%%bits_to_status(Pns,Len) ->
%%    <<<<Y:Len>> || <<X:1>> <= Pns, Y <- [X]>>.
%%
%%get_binary_block_n(Binary, N, Size) when is_binary(Binary) and is_integer(Size) and is_integer(N) ->
%%    Pos = (N - 1) * Size,
%%    <<_:Pos, Value:Size, _/bits>> = Binary,
%%    Value.
%%set_binary_block_n(Binary, N, Value, Size) when is_binary(Binary) and is_integer(Size) and is_integer(N) ->
%%    Pos = (N - 1) * Size,
%%    <<Head:Pos, _:Size, Tail/bits>> = Binary,
%%    <<Head:Pos, Value:Size, Tail/bits>>.
%%
%%get_next_pn_bits(Pn, Pns) when is_binary(Pns) ->
%%    get_next_pn_bits_(Pn, Pns).

%%get_next_pn_bits_(Pn, Pns) when erlang:bit_size(Pns) == Pn ->
%%    get_next_pn_bits_(0, Pns);

%%get_next_pn_bits_(Pn, Pns) when erlang:bit_size(Pns) >= Pn ->
%%    case Pns of
%%        <<>> -> 0;
%%        <<_:Pn, 0:1>> -> Pn;
%%        <<_:Pn, 1:1>> -> Pn + 1;
%%        <<_:Pn,1:1,_/bits>> -> Pn + 1;
%%        <<_:Pn,0:1,_/bits>> -> get_next_pn_bits_(Pn + 1,Pns);
%%        _ -> ?LOG(info,"Pn ~p, Pns ~p",[Pn, Pns]),Pn
%%    end;
%%
%%get_next_pn_bits_(Pn, _Pns) ->
%%    Pn.

%%binary_to_bits_0_list(Binary) when is_binary(Binary) ->
%%    List = [X || <<X:1>> <= Binary],
%%    {ZeroList, _} = lists:foldl(
%%        fun(X, {Acc, Pos}) ->
%%            case X of
%%                1 -> {Acc, Pos + 1};
%%                0 -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
%%            end
%%        end, {[], 0}, List),
%%    ZeroList.

%%binary_to_bit_1_list(<<>>) ->
%%    [0];
%%binary_to_bit_1_list(Binary) when is_binary(Binary) ->
%%    List = [X || <<X:1>> <= Binary],
%%    {NoZeroList, _} = lists:foldl(
%%        fun(X, {Acc, Pos}) ->
%%            case X of
%%                0 -> {Acc, Pos + 1};
%%                _ -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
%%            end
%%        end, {[], 0}, List),
%%    NoZeroList.
%%
%%get_binary_bits_n(Binary, N) when is_binary(Binary) ->
%%    get_binary_bits_n_(Binary, N).
%%
%%get_binary_bits_n_(Binary, N) when erlang:bit_size(Binary) >= N ->
%%    Pos = N - 1,
%%    <<_:Pos, Value:1, _/bits>> = Binary,
%%    Value;
%%get_binary_bits_n_(_Binary, _N)  ->
%%    0.

%%set_binary_bits_n(Binary, N, Value) when is_binary(Binary) ->
%%    set_binary_bits_n_(Binary, N, Value).

%%set_binary_bits_n_(Binary, N, Value) when erlang:bit_size(Binary) >= N ->
%%    Pos = N - 1,
%%    <<Head:Pos, _:1, Tail/bits>> = Binary,
%%    <<Head:Pos, Value:1, Tail/bits>>;
%%set_binary_bits_n_(Binary, _N, _Value) ->
%%    Binary.

%%binary_bits_sum(Binary) when is_binary(Binary) ->
%%    lists:sum([X || <<X:1>> <= Binary]).
%%
%%binary_bits_zero_list(Binary) when is_binary(Binary) ->
%%    List = [X || <<X:1>> <= Binary],
%%    {ZeroList, _} = lists:foldl(
%%        fun(X, {Acc, Pos}) ->
%%            case X of
%%                1 -> {Acc, Pos + 1};
%%                0 -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
%%            end
%%        end, {[], 0}, List),
%%    ZeroList.
%%
%%binary_bits_nozero_list(Binary) when is_binary(Binary) ->
%%    List = [X || <<X:1>> <= Binary],
%%    {NoZeroList, _} = lists:foldl(
%%        fun(X, {Acc, Pos}) ->
%%            case X of
%%                0 -> {Acc, Pos + 1};
%%                _ -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
%%            end
%%        end, {[], 0}, List),
%%    NoZeroList.
%%
%%binary_bits_zip(Binary) when is_binary(Binary) ->
%%    ReverBin = list_to_binary(lists:reverse(binary_to_list(Binary))),
%%    list_to_binary(lists:reverse(binary_to_list(zip_bin(ReverBin)))).

%%zip_bin(<<0:8, Binary/binary>>) ->
%%    zip_bin(Binary);
%%zip_bin(Binary) ->
%%    Binary.
%%
%%vcon() ->
%%    case dgiot_parse:query_object(<<"Vcon">>,
%%        #{<<"count">> => 1, <<"limit">> => 0}) of
%%        {ok, #{<<"count">> := N}} ->
%%            N;
%%        _ ->
%%            0
%%    end.

%%meter() ->
%%    case dgiot_parse:query_object(<<"Smartmeter">>,
%%        #{<<"count">> => 1, <<"limit">> => 0}) of
%%        {ok, #{<<"count">> := 0}} ->
%%            vcon() * 600;
%%        {ok, #{<<"count">> := N}} ->
%%            N;
%%        _ ->
%%            vcon() * 600
%%    end.

%%online_meter() ->
%%    case dgiot_parse:query_object(<<"Smartmeter">>,
%%        #{<<"count">> => 1, <<"limit">> => 0}) of
%%        {ok, #{<<"count">> := 0}} ->
%%            vcon() * 600;
%%        {ok, #{<<"count">> := N}} ->
%%            N;
%%        _ ->
%%            vcon() * 600
%%    end.
%%
%%tq() ->
%%    100.
%%%%    case dgiot_parse:aggregate_object(<<"Smartmeter">>, #{<<"distinct">> => <<"tq">>, <<"limit">> => 0}) of
%%%%        {ok, #{<<"results">> := Ls}} ->
%%%%            length(Ls);
%%%%        _ ->
%%%%            100
%%%%    end.
%%
%%deveui() ->
%%    100000.
%%%%    case dgiot_parse:aggregate_object(<<"Smartmeter">>, #{<<"distinct">> => <<"deveui">>, <<"limit">> => 0}) of
%%%%        {ok, #{<<"results">> := Ls}} ->
%%%%            length(Ls);
%%%%        _ ->
%%%%            1000000
%%%%    end.
