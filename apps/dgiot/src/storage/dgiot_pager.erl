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

-module(dgiot_pager).
-author("johnliu").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([page/6, all/3, query_table/3, match_count/2, match_count/4]).


%%--------------------------------------------------------------------
%% Table Query and Pagination
%%--------------------------------------------------------------------

page(Handle, Filter, PageNo, PageSize, RowFun, Order) ->
    {Q, TotalNum} = get_query(Handle, Filter, Order, fun(_X, Sum) -> 1 + Sum end, 0),
    Rows =
        case TotalNum > 0 of
            true ->
                query_table(Q, PageNo, PageSize);
            false ->
                []
        end,
    #{
        <<"totalNum">> => TotalNum,
        <<"totalPage">> => total_page(TotalNum, PageSize),
        <<"result">> => do_rows(Rows, RowFun)
    }.


all(Handle, Filter, Order) ->
    {Q, TotalNum} = get_query(Handle, Filter, Order, fun(_X, Sum) -> 1 + Sum end, 0),
    case TotalNum > 0 of
        true ->
            query_table(Q, 1, TotalNum);
        false ->
            []
    end.


match_count(Handle, Filter) ->
    match_count(Handle, Filter, fun(_X, Sum) -> 1 + Sum end, 0).

match_count(Handle, Filter, Fun, Acc) ->
    {_Qh, TotalNum} = get_query(Handle, Filter, none, Fun, Acc),
    TotalNum.


query_table(Qh, PageNo, PageSize) ->
    Cursor = qlc:cursor(Qh),
    case PageNo > 1 of
        true ->
            qlc:next_answers(Cursor, (PageNo - 1) * PageSize);
        false ->
            ok
    end,
    Rtn = qlc:next_answers(Cursor, PageSize),
    qlc:delete_cursor(Cursor),
    Rtn.



get_query(Handle, Filter, Order, Fun, Acc) ->
    Q = qlc:q([E || E <- Handle, Filter(E)]),
    NewQ =
        case is_function(Order) of
            true ->
                qlc:sort(Q, [{order, Order}]);
            false ->
                case Order of
                    {KeyPos, Options} ->
                        qlc:e(qlc:keysort(KeyPos, Q, Options));
                    _ ->
                        Q
                end
        end,
    R = qlc:fold(Fun, Acc, NewQ),
    {Q, R}.


do_rows(Rows, RowFun) ->
    case RowFun of
        {direct, RowFun1} ->
            RowFun1(Rows);
        _ ->
            lists:foldr(
                fun(Row, Acc) ->
                    case RowFun(Row) of
                        [] -> Acc;
                        Rtn -> [Rtn | Acc]
                    end
                end, [], Rows)
    end.


total_page(TotalNum, PageSize) ->
    case PageSize of
        0 ->
            0;
        _ ->
            case TotalNum rem PageSize of
                0 -> TotalNum div PageSize;
                _ -> (TotalNum div PageSize) + 1
            end
    end.
