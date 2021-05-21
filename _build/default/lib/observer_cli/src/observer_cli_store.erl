%%% @author zhongwen <zhongwencool@gmail.com>

-module(observer_cli_store).

%% API
-export([start/0]).
-export([update/3]).
-export([lookup_pos/2]).
-export([lookup_row/1]).

-define(LOOKUP_PID, lookup_pid).
-define(LOOKUP_PID_RES, lookup_pid_result).
-define(LOOKUP_ROW, lookup_row).
-define(LOOKUP_ROW_RES, lookup_row_result).
-define(UPDATE_TOP_N, update_top_n).

-spec start() -> pid().
start() ->
    spawn_link(fun() -> loop({1, []}) end).

-spec update(pid(), pos_integer(), list()) -> ok.
update(StorePid, Row, TopNList) ->
    erlang:send(StorePid, {?UPDATE_TOP_N, Row, TopNList}),
    ok.

-spec lookup_pos(pid(), pos_integer()) -> {pos_integer(), pid()}.
lookup_pos(StorePid, CurPos) ->
    erlang:send(StorePid, {?LOOKUP_PID, CurPos, self()}),
    receive
        {?LOOKUP_PID_RES, Res} -> Res
    end.

-spec lookup_row(pid()) -> pos_integer().
lookup_row(StorePid) ->
    erlang:send(StorePid, {?LOOKUP_ROW, self()}),
    receive
        {?LOOKUP_ROW_RES, Res} -> Res
    end.

loop({Row, PidList} = TopList) ->
    NewTopList =
        receive
            {?UPDATE_TOP_N, NewRow, NewPidList} ->
                {NewRow, NewPidList};
            {?LOOKUP_PID, Pos, From} ->
                Res =
                    case PidList of
                        [] ->
                            {error, undefined};
                        _ ->
                            case lists:keyfind(Pos, 1, PidList) of
                                false -> lists:last(PidList);
                                Item -> Item
                            end
                    end,
                erlang:send(From, {?LOOKUP_PID_RES, Res}),
                TopList;
            {?LOOKUP_ROW, From} ->
                erlang:send(From, {?LOOKUP_ROW_RES, Row}),
                TopList
        end,
    loop(NewTopList).
