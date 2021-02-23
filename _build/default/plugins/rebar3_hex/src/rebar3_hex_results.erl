-module(rebar3_hex_results).

-export([errors_to_string/1, print_table/1]).

-include("rebar3_hex.hrl").

errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string(Map) when is_map(Map) ->
    errors_to_string(maps:to_list(Map));
errors_to_string({<<"inserted_at">>, E}) ->
    lists:flatten(io_lib:format("Inserted At: ~s~n", [E]));
errors_to_string({<<"requirements">>,  Rs}) ->
    lists:flatten(["Requirements could not be computed\n",
                  [io_lib:format("~s\n~20.20c\n~s\n",[P,$-, R]) || {P, R} <- maps:to_list(Rs)]]);
errors_to_string({Key, Value}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Value)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Values)]) || Values <- Errors]).

print_table(Rows) ->
    Table = table(Rows),
    io:fwrite(Table),
    ok.

underline_emphasis(Item) ->
    io_lib:format("\e[1m\e[00m\e[4m~ts\e[24m", [Item]).

% Returns a str, expects first row to be a header
table(Rows) ->
    [Header | Body] = align_rows(Rows),
    Table = [pretty_header(Header), ""] ++ Body,
    lists:foldl(fun(Row, Acc) ->
                        Acc ++ [io_lib:fwrite("~s~n", [lists:flatten(Row)])]
                end,
                [],
                Table).

pretty_header(Header) ->
    lists:map(fun(W) ->
                      [Value, Space] = rebar3_hex_io:str_split(W, " "),
                      underline_emphasis(Value) ++ " "  ++ Space  end,
              Header).

align_rows(Rows) ->
    WidestCells = widest_cells(Rows),
    [align_cells(R, WidestCells) || R <- Rows].

align_cells(Row, WidestCells) ->
    Padded = rpad_row(Row, length(WidestCells), ""),
    [ string:left(Cell, Length + 2, $\s)
      || {Cell, Length} <- lists:zip(Padded, WidestCells)].

widest_cells(Rows) ->
    lists:foldl( fun(Row, Acc) ->
                         CellLengths = [length(C) || C <- Row ],
                         Widest = lists:max([length(Acc), length(CellLengths)]),
                         Padded = rpad_row(CellLengths, Widest, 0),
                         WidestPadded = rpad_row(Acc, Widest, 0),
                         [ lists:max([A, B]) || {A, B} <- lists:zip(Padded, WidestPadded)]
                 end,
                 [],
                 Rows).

rpad_row(L, Length, Elem) ->
    L ++ lists:duplicate(Length - length(L), Elem).
