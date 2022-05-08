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
-export([test/0, with/2, get/2, merge/2]).

merge(_Data, NewData) ->
    Keys = maps:keys(NewData),
    Keys.

%%    maps:fold(fun(K, V, Acc) ->
%%        case maps:find(K, Class) of
%%            error -> Acc;
%%            {ok, Value} when is_map(Value) ->
%%                Acc#{K => maps:merge(Value, V)};
%%            _ ->
%%                Acc#{K => V}
%%        end
%%              end,
%%        #{}, maps:without([<<"id">>], Args));

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

test() ->
    Keys = [<<"content.i_out">>, <<"content.i_in">>],
    Data = #{<<"content">> => #{<<"i_out">> => 1, <<"i_in">> => 9}},
    with(Keys, Data).
