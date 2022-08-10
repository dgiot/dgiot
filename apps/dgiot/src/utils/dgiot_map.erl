%%%-------------------------------------------------------------------
%%% @author jonhl
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 5月 2021 7:08
%%%-------------------------------------------------------------------
-module(dgiot_map).
-author("jonhl").
-export([with/2, get/2, merge/2, flatten/1, flatten/2]).
-export([test_get/0, test_merge/0]).


-export([unflatten/1, unflatten/2]).

unflatten(Data) ->
    unflatten(Data, "_").

unflatten(List, Link) when is_list(List) ->
    lists:foldl(
        fun(X, Acc) ->
            Acc ++ [unflatten(X, Link)]
        end, [], List);

unflatten(Map, Link) when is_map(Map) ->
    maps:fold(
        fun(L, V, Acc) ->
            KList = lists:reverse(re:split(L, Link)),
            NewMap = get_map(KList, V),
            merge(Acc, NewMap)
        end, #{}, Map);

unflatten(Data, _) ->
    Data.

get_map(KList, V) ->
    lists:foldl(
        fun(X, Acc) ->
            case maps:size(Acc) of
                0 ->
                    #{X => V};
                _ ->
                    #{X => Acc}
            end
        end, #{}, KList).

flatten(Map) ->
    flatten(Map, <<"_">>).

flatten(Map, Link) ->
    case is_map(Map) of
        true ->
            maps:fold(
                fun(K, V, Acc) ->
                    maps:merge(Acc, flatten(<<K/binary>>, V, Link))
                end, #{}, Map);
        false ->
            {error, <<"wrong_type">>}

    end.

flatten(Head, Map, Link) ->
    case is_map(Map) of
        true ->
            maps:fold(
                fun(K, V, Acc) ->
                    maps:merge(Acc, flatten(<<Head/binary, Link/binary, K/binary>>, V, Link))
                end, #{}, Map);
        false ->
            #{<<Head/binary>> => Map}

    end.


merge(Data, NewData) ->
    maps:fold(fun
                  (NewKey, NewValue, Acc) ->
                      case maps:find(NewKey, Acc) of
                          {ok, Value} when is_map(Value) and is_map(NewValue) ->
                              Acc#{NewKey => merge(Value, NewValue)};
                          _ ->
                              Acc#{NewKey => NewValue}
                      end
              end,
        Data, NewData).

with(Keys, Data) ->
    with(Keys, Data, #{}).

with([], _Data, Acc) ->
    Acc;
with([Key | Keys], Data, Acc) ->
    Map = get(Key, Data),
    with(Keys, Data, maps:merge(Acc, Map)).

get(Key, Data) ->
    case re:split(Key, <<"[.]">>, [{return, binary}, trim]) of
        List when length(List) == 1 ->
            case Data of
                #{Key := Value} ->
                    #{Key => Value};
                _ ->
                    #{}
            end;
        JsonKey ->
            case value(JsonKey, Data) of
                undefined -> #{};
                Value -> #{Key => Value}
            end
    end.

value([], Value) ->
    Value;
value([Key | Keys], Data) when is_map(Data) ->
    case Data of
        #{Key := Value} ->
            value(Keys, Value);
        _ ->
            undefined
    end;
value(_, _Value) ->
    undefined.

test_get() ->
    Keys = [<<"content.i_out">>, <<"content.i_in">>],
    Data = #{<<"content">> => #{<<"i_out">> => 1, <<"i_in">> => 9}},
    with(Keys, Data).

test_merge() ->
    A = #{1 => #{1 => 11}},
    B = #{1 => #{1 => 2, 2 => 3}, 2 => #{4 => 5}},
    merge(A, B).
