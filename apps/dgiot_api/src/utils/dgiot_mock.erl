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

-module(dgiot_mock).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([mock/5, mock/3, mock/4, mock_r/4, mock_r/2, mock_r/3, test/0]).
-export([do_mock/2, do_mock_data/1]).

%% 构造请求回应
do_mock(#{200 := ResultRule}, _Req) ->
    {200, do_mock_data(ResultRule)};
do_mock(#{201 := ResultRule}, _Req) ->
    {201, do_mock_data(ResultRule)};
do_mock(_, _Req) ->
    {200, #{msg => <<"this is mock data!">>}}.

do_mock_data(#{<<"name">> := Name, <<"type">> := <<"string">>}) when
    Name == <<"createdAt">>; Name == <<"updatedAt">> ->
    Now = dgiot_datetime:nowstamp(),
    Format = <<"YY-MM-DDTHH:NN:SSZ">>,
    Value = dgiot_datetime:format(mock_r({Now - 10 * 365 * 86400, Now}, Now, []), Format),
    #{Name => Value};

do_mock_data(#{<<"name">> := <<"phone">>, <<"type">> := <<"string">>}) ->
    Value = mock_r({13800000000, 13899999999}, 0, []),
    #{<<"phone">> => integer_to_binary(Value) };

do_mock_data(#{<<"name">> := <<"email">>, <<"type">> := <<"string">>}) ->
    Value = mock_r({5, 10}, lists:seq($A, $Z) ++ lists:seq($a, $z), []) ++ "@" ++ "example.com",
    #{<<"email">> => list_to_binary(Value)};

do_mock_data(#{<<"name">> := <<"password">>, <<"type">> := <<"string">>}) ->
    Value = mock_r({6, 12}, <<"*">>, []),
    #{<<"password">> => Value};

do_mock_data(#{<<"name">> := Name, <<"type">> := <<"string">>} = Item) ->
    Min = maps:get(<<"minLength">>, Item, 5),
    Max = maps:get(<<"maxLength">>, Item, 10),
    Value = mock_r({Min, Max}, lists:seq($A, $Z) ++ lists:seq($a, $z), []),
    #{Name => list_to_binary(Value)};

do_mock_data(#{<<"name">> := Name, <<"type">> := <<"boolean">>}) ->
    Value = mock_r({1, 4}, true, []),
    #{Name => Value};

do_mock_data(#{<<"name">> := Name, <<"type">> := Type} = Item)
    when Type == <<"integer">>; Type == <<"number">> ->
    Min = maps:get(<<"minimum">>, Item, 0),
    Max = maps:get(<<"maximum">>, Item, 1000000),
    Value = mock_r({Min, Max}, true, []),
    #{Name => Value};

do_mock_data(#{ <<"type">> := <<"object">>} = Item) ->
    maps:fold(
        fun(Name, Rule, Object) ->
            maps:merge(Object, do_mock_data(Rule#{ <<"name">> => Name }))
        end, #{}, maps:get(<<"properties">>, Item, #{}));

do_mock_data(#{ <<"name">> := Name, <<"type">> := <<"array">>, <<"items">> := Items}) ->
    Fun = fun() -> do_mock_data(Items) end,
    #{ Name => mock_r({3, 10}, Fun, [])}.






mock(Name, Rule, Value, Opts) ->
    #{Name => mock_r(Rule, Value, Opts)}.

mock(Mock, Name, Rule, Value, Opts) ->
    #{Name => mock_r(Mock, Rule, Value, Opts)}.

mock(Name, Fun, Opts) ->
    #{Name => mock_r(Fun, Opts)}.


%%'name|min-max.dmin-dmax': number
%%生成一个浮点数，整数部分大于等于 min、小于等于 max，小数部分保留 dmin 到 dmax 位
mock_r({{Min, Max}, {DMin, DMax}}, Value, _Opts) when is_integer(Value) ->
    X = DMin + rand:uniform(DMax - DMin),
    N = math:pow(10, X),
    F = round(rand:uniform() * N) / N,
    Min + rand:uniform(Max - Min) + F;

%% name|min-max': number
%%生成一个大于等于 min、小于等于 max 的整数，属性值 number 只是用来确定类型
mock_r({Min, Max}, Value, _Opts) when is_integer(Value) ->
    Min + rand:uniform(Max - Min);

%% 'name|+1': number
%%属性值自动加 1，初始值为 number
mock_r("+1", Value, _Opts) when is_integer(Value) ->
    Value + 1;

%% name|min-max': string
%% 通过重复 string 生成一个字符串，重复次数大于等于 min，小于等于 max
mock_r({Min, Max}, Value, Opts) when is_binary(Value) ->
    Count = Min + rand:uniform(Max - Min),
    mock_r(Count, Value, Opts);

%%'name|1': boolean
%%随机生成一个布尔值，值为 true 的概率是 1/2，值为 false 的概率同样是 1/2。
mock_r("1", Value, Opts) when is_boolean(Value) ->
    mock_r({1, 1}, Value, Opts);

%% 'name|min-max': value
%%随机生成一个布尔值，值为 value 的概率是 min / (min + max)，值为 !value 的概率是 max
mock_r({Min, Max}, Value, _Opts) when is_boolean(Value) ->
    case rand:uniform(Max + Min) =< Min of
        true ->
            Value;
        false ->
            not Value
    end;

%%'name|count': string
%%通过重复 string 生成一个字符串，重复次数等于 count
mock_r(Count, Value, _Opts) when is_binary(Value) ->
    list_to_binary(lists:concat(lists:duplicate(Count, binary_to_list(Value))));

%%
%%'name|1': array
%%从属性值 array 中随机选取 1 个元素，作为最终值。
mock_r("1", List, _Opts) when is_list(List), length(List) > 0 ->
    lists:nth(rand:uniform(length(List)), List);

%%'name|count': array
%%通过重复属性值 array 生成一个新数组，重复次数为 count。
mock_r(Count, List, _Opts) when is_list(List), length(List) > 0, is_integer(Count) ->
    lists:map(fun(_) -> lists:nth(rand:uniform(length(List)), List) end, lists:seq(1, Count));

%%'name|min-max': array
%%通过重复属性值 array 生成一个新数组，重复次数大于等于 min，小于等于 max。
mock_r({Min, Max}, List, _Opts) when is_list(List), length(List) > 0 ->
    Count = Min + rand:uniform(Max - Min),
    mock_r(Count, List, _Opts);

%%'name|min-max': object
%%从属性值 object 中随机选取 min 到 max 个属性。
mock_r({Min, Max}, Map, _Opts) when is_map(Map) ->
    Count = Min + rand:uniform(Max - Min),
    mock_r(Count, Map, _Opts);

%%'name|count': object
%%从属性值 object 中随机选取 count 个属性。
mock_r(Count, Map, _Opts) when is_map(Map) ->
    {_, New} = lists:foldl(
        fun(_, {Acc1, Acc2}) ->
            case length(Acc1) == 0 of
                true ->
                    {Acc1, Acc2};
                false ->
                    Key = lists:nth(rand:uniform(length(Acc1)), Acc1),
                    {proplists:delete(Key, Acc1), [Key | Acc2]}
            end
        end, {maps:keys(Map), []}, lists:seq(1, Count)),
    maps:with(New, Map);

mock_r({Min, Max}, Fun, _Opts) when is_function(Fun) ->
    Count = Min + rand:uniform(Max - Min),
    mock_r(Count, Fun, _Opts);

mock_r(Count, Fun, Opts) when is_function(Fun) ->
    [mock_r(Fun, Opts) || _ <- lists:seq(1, Count)].

%%'name|+1': array
%%从属性值 array 中顺序选取 1 个元素，作为最终值
mock_r(Mock, "+1", List, _Opts) when is_list(List), length(List) > 0 ->
    Index =
        case maps:get(idx, Mock, 0) + 1 of
            Idx when Idx > length(List) -> 1;
            Idx -> Idx
        end,
    {
        Mock#{idx => Index}, lists:nth(Index, List)
    }.


%%'name': function
%%执行函数 function，取其返回值作为最终的属性值，函数的上下文为属性 'name' 所在的对象。
mock_r(Fun, _Opts) when is_function(Fun) ->
    Fun().




test() ->
    lists:foreach(
        fun(Args) ->
            Result = apply(?MODULE, mock, Args ++ [[]]),
            ?LOG(info,"mock(~p) => ~n~p~n~n", [Args, Result])
        end, [
            [float, {{1, 3}, {1, 4}}, 0],
            [int, {1, 10}, 0],
            [int, "+1", 2],
            [string, {1, 5}, <<"zwx">>],
            [string, 10, <<"zwx">>],
            [bool, "1", true],
            [bool, {1, 3}, true],
            [array, 1, [<<"a">>, <<"b">>, <<"c">>]],
            [array, 3, [<<"a">>, <<"b">>, <<"c">>]],
            [#{}, array, "+1", [<<"a">>, <<"b">>, <<"c">>]],
            [array, {1, 3}, [<<"a">>, <<"b">>, <<"c">>]],
            [object, 2, #{name1 => zwx, name2=>zwx2, name3=>zwx3}],
            [object, {1, 3}, #{name1 => zwx, name2=>zwx2, name3=>zwx3}],
            [function, fun() -> <<"zwx">> end]
        ]).

