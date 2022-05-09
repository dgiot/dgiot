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
-export([with/2, get/2, merge/2]).
-export([test_get/0, test_merge/0]).

merge(Data, NewData) ->
    maps:fold(fun
                  (NewKey, NewValue, Acc) ->
                      case maps:find(NewKey, Acc) of
                          {ok, Value} when is_map(Value) ->
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