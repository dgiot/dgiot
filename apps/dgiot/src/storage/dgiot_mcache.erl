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

-module(dgiot_mcache).

-author("johnliu").
-include("dgiot_mnesia.hrl").

-export([insert/1, delete/1, lookup/1, match_object/1, match_object/2, page/5]).
-export([insert/2, insert/3, delete/2, lookup/2, match_object/3, page/6]).


insert(Value) ->
    insert(?MCACHE, Value).

insert(TB, {Key, Value}) ->
    insert(TB, Key, Value).

insert(TB, Key, Value) ->
    Result = dgiot_mnesia:insert(TB, #dgiot_mcache{key = Key, value = Value}),
    result(Result).


lookup(Key) ->
    lookup(?MCACHE, Key).

lookup(TB, Key) ->
    Result = dgiot_mnesia:lookup(TB, Key),
    case result(Result) of
        {error, Reason} ->
            {error, Reason};
        {ok, [#dgiot_mcache{value = Value}]} ->
            {ok, Value}
    end.

delete(Key) ->
    delete(?MCACHE, Key).

delete(TB, Key) ->
    Result = dgiot_mnesia:delete(TB, Key),
    result(Result).


match_object({PKey, PValue}) ->
    match_object(?MCACHE, {PKey, PValue}, fun(Row) -> Row end).

match_object({PKey, PValue}, RowFun) ->
    match_object(?MCACHE, {PKey, PValue}, RowFun).

match_object(TB, {PKey, PValue}, RowFun) ->
    Result = dgiot_mnesia:match_object(TB, #dgiot_mcache{key = PKey, value = PValue}),
    case result(Result) of
        {ok, Records} ->
            [RowFun({Key, Value})||#dgiot_mcache{key = Key, value = Value}<-Records];
        {error, empty} ->
            [];
        {error, Reason} ->
            {error, Reason}
    end.


page(PageNo, PageSize, Filter, RowFun, Order) ->
    page(?MODULE, PageNo, PageSize, Filter, RowFun, Order).

page(TB, PageNo, PageSize, Filter, RowFun, Order) ->
    NewRowFun =
        fun(#dgiot_mcache{key = Key, value = Value}) ->
            RowFun({Key, Value})
        end,
    NewFilter =
        fun(#dgiot_mcache{key = Key, value = Value}) ->
            Filter({Key, Value})
        end,
    NewOrder =
        case is_function(Order) of
            true ->
                fun(#dgiot_mcache{key = Key, value = Value}, #dgiot_mcache{key = Key1, value = Value1}) ->
                    Order({Key, Value}, {Key1, Value1})
                end;
            _ ->
                Order
        end,
    Result = dgiot_mnesia:page(TB, PageNo, PageSize, NewFilter, NewRowFun, NewOrder),
    result(Result).


result({atomic, ok}) -> true;
result({atomic, []}) -> {error, empty};
result({aborted, Reason}) -> {error, Reason};
result({atomic, Result}) -> {ok, Result};
result(Result) -> Result.
