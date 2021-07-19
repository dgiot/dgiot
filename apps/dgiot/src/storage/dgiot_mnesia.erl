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

-module(dgiot_mnesia).

-author("johnliu").
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include("dgiot_mnesia.hrl").
-include("dgiot.hrl").
-include("logger.hrl").
-include("types.hrl").
-include_lib("ekka/include/ekka.hrl").
-logger_header("[Mnesia]").

%% Mnesia bootstrap
-export([mnesia/1]).

-boot_mnesia({mnesia, [boot]}).
-copy_mnesia({mnesia, [copy]}).

%% API
-export([
    page/5,
    lookup/1,
    insert/2,
    delete/1,
    select_count/0,
    select_where/1,
    select_limit/1,
    select_object/2,
    match_object/1,
    match_object/2,
    search/0,
    search/2,
    search/3
]).

-export([start_link/2]).

%% Mnesia APIs
-export([add_mnesia/1
    , add_mnesia/2
    , do_add_mnesia/1
    , do_add_mnesia/2
]).

-export([delete_mnesia/1
    , delete_mnesia/2
    , do_delete_mnesia/1
    , do_delete_mnesia/2
]).

-export([match_mnesia/1
    , lookup_mnesia/1
    , has_mnesia/1
    , maybe_trans/2
]).

-export([print_mnesia/1]).

-export([keys/0]).

%% gen_server callbacks
-export([init/1
    , handle_call/3
    , handle_cast/2
    , handle_info/2
    , terminate/2
    , code_change/3
]).

%%--------------------------------------------------------------------
%% Mnesia bootstrap
%%--------------------------------------------------------------------

mnesia(boot) ->
    ok = ekka_mnesia:create_table(?MNESIA_TAB, [
        {type, set},
        {ram_copies, [node()]},
        {record_name, mnesia},
        {attributes, record_info(fields, mnesia)},
        {storage_properties, [{ets, [{read_concurrency, true},
            {write_concurrency, true}]}]}]);
mnesia(copy) ->
    ok = ekka_mnesia:copy_table(?MNESIA_TAB, ram_copies).

%%--------------------------------------------------------------------
%% Start a Mnesia
%%--------------------------------------------------------------------

-spec(start_link(atom(), pos_integer()) -> startlink_ret()).
start_link(Pool, Id) ->
    gen_server:start_link({local, dgiot_misc:proc_name(?MODULE, Id)},
        ?MODULE, [Pool, Id], [{hibernate_after, 1000}]).

%%--------------------------------------------------------------------
%% Mnesia APIs
%%--------------------------------------------------------------------
-spec(lookup(dgiot_type:key()) -> ok | {error, term()}).
lookup(Key) ->
    lookup(?MNESIA_TAB, Key).

-spec(lookup(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
lookup(Tab, Key) ->
    Result = mnesia:transaction(fun mnesia:read/1, [{Tab, Key}]),
    result(Result).

%% todo 分布式节点是否需要远程调用？
%%lookup(Node, ProductId, DevAddr) ->
%%%%    case rpc:call(Node, ?MODULE, lookup, [Node, ProductId, DevAddr]) of
%%%%        {badrpc, Reason} ->
%%%%            {error, Reason};
%%%%        Res ->
%%%%            Res
%%%%    end.
-spec(insert(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
insert(Key, Value) ->
    insert(?MNESIA_TAB, Key, Value).

insert(TB, Key, Value) ->
    insert_(TB, #mnesia{key = Key, value = Value}).

insert_(TAB, Record) ->
    F = fun() -> mnesia:write(TAB, Record, write) end,
    Result = mnesia:transaction(F),
    result(Result).

delete(Key) ->
    delete(?MNESIA_TAB, Key).

delete(TAB, Key) ->
    F =
        fun() ->
            mnesia:delete({TAB, Key})
        end,
    Result = mnesia:transaction(F),
    result(Result).


select_count() ->
    select_count(?MNESIA_TAB).

select_count(TAB) ->
    F =
        fun() ->
            mnesia:table_info(TAB, size)
        end,
    Result = mnesia:transaction(F),
    result(Result).

select_limit(Limit) ->
    select_limit(?MNESIA_TAB, Limit).

select_limit(TAB, Limit) ->
    F =
        fun() ->
            Q = qlc:q([E || E <- mnesia:table(TAB)]),
            QC = qlc:cursor(Q),
            qlc:next_answers(QC, Limit)
        end,
    Result = mnesia:transaction(F),
    result(Result).

match_object({PKey, PValue}) ->
    match_object(?MNESIA_TAB, {PKey, PValue}, fun(Row) -> Row end);

match_object({PKey, PValue, RowFun}) ->
    match_object(?MNESIA_TAB, {PKey, PValue}, RowFun).

match_object({PKey, PValue}, RowFun) ->
    match_object(?MNESIA_TAB, {PKey, PValue}, RowFun);

match_object(TB, Record) ->
    F =
        fun() ->
            mnesia:match_object(TB, Record, read)
        end,
    Result = mnesia:transaction(F),
    result(Result).

match_object(TB, {PKey, PValue}, RowFun) ->
    Result = match_object(TB, #mnesia{key = PKey, value = PValue}),
    case result(Result) of
        {ok, Records} ->
            [RowFun({Key, Value}) || #mnesia{key = Key, value = Value} <- Records];
        {error, empty} ->
            [];
        {error, Reason} ->
            {error, Reason}
    end.


%% where qlc:q([E || E <- mnesia:table(TAB), E#tabrecord.id > 5])
%% order fun(A, B) -> B#tabrecord.id > A#tabrecord.id end,
select_where({Filter}) ->
    select_where(?MNESIA_TAB, Filter);
select_where({Filter, Order}) ->
    select_where(?MNESIA_TAB, Filter, Order).

select_where(TB, Filter) ->
    select_where(TB, Filter, none).

select_where(TB, Filter, Order) ->
    F =
        fun() ->
            Q = qlc:q([E || E <- mnesia:table(TB), Filter(E)]),
            NewQ =
                case is_function(Order) of
                    true ->
                        qlc:sort(Q, [{order, Order}]);
                    false ->
                        Q
                end,
            qlc:e(NewQ)
        end,
    Result = mnesia:transaction(F),
    result(Result).

select_object(Filter, Order) ->
    select_object(?MNESIA_TAB, Filter, Order).

select_object(TB, Filter, Order) ->
    F =
        fun() ->
            dgiot_pager:all(mnesia:table(TB), Filter, Order)
        end,
    Result = mnesia:transaction(F),
    result(Result).

search() ->
    Fun = fun(X) ->
        {_, K, V} = X,
        #{K => V}
          end,
    search(Fun,#{<<"start">> => 0, <<"len">> => 5}).

search(Fun,Page) ->
    search(?MNESIA_TAB, Fun, Page).

search(Name, Fun, Page) ->
    ets:safe_fixtable(Name, true),
    DataSet = search(Name, Fun, ets:first(Name), [], Page#{<<"count">> => 0}),
    ets:safe_fixtable(Name, false),
    DataSet.

search(_, _, '$end_of_table', Acc, Page) ->
    Page#{<<"data">> => lists:reverse(Acc), <<"len">> => length(Acc)};

search(Name, Fun, Key, Acc, Page = #{<<"start">> := Start, <<"len">> := Len, <<"count">> := Count}) when Count >= Start, Count < Start + Len ->
    case ets:lookup(Name, Key) of
        [Row | _] ->
            case Fun(Row) of
                false ->
                    search(Name, Fun, ets:next(Name, Key), Acc, Page#{<<"count">> => Count});
                Element ->
                    search(Name, Fun, ets:next(Name, Key), [Element | Acc], Page#{<<"count">> => Count + 1})
            end;
        _ ->
            search(Name, Fun, ets:next(Name, Key), Acc, Page#{<<"count">> => Count})
    end;

search(Name, Fun, Key, Acc, Page = #{<<"count">> := Count}) ->
    case ets:lookup(Name, Key) of
        [Row | _] ->
            case Fun(Row) of
                false ->
                    search(Name, Fun, ets:next(Name, Key), Acc, Page#{<<"count">> => Count});
                _Element ->
                    search(Name, Fun, ets:next(Name, Key), Acc, Page#{<<"count">> => Count + 1})
            end;
        _ ->
            search(Name, Fun, ets:next(Name, Key), Acc, Page#{<<"count">> => Count})
    end.

page(PageNo, PageSize, Filter, RowFun, Order) ->
    page(?MNESIA_TAB, PageNo, PageSize, Filter, RowFun, Order).

page(TB, PageNo, PageSize, Filter, RowFun, Order) ->
    NewRowFun =
        fun(#mnesia{key = Key, value = Value}) ->
            RowFun({Key, Value})
        end,
    NewFilter =
        fun(#mnesia{key = Key, value = Value}) ->
            Filter({Key, Value})
        end,
    NewOrder =
        case is_function(Order) of
            true ->
                fun(#mnesia{key = Key, value = Value}, #mnesia{key = Key1, value = Value1}) ->
                    Order({Key, Value}, {Key1, Value1})
                end;
            _ ->
                Order
        end,
    Result = dgiot_pager:page(TB, PageNo, PageSize, NewFilter, NewRowFun, NewOrder),
    result(Result).


result({atomic, ok}) -> true;
result({atomic, []}) -> {error, empty};
result({aborted, Reason}) -> {error, Reason};
result({atomic, Result}) -> {ok, Result};
result(Result) -> Result.

-spec(add_mnesia(dgiot_type:key()) -> ok | {error, term()}).
add_mnesia(Key) ->
    add_mnesia(Key, node()).

-spec(add_mnesia(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
add_mnesia(Key, Value) ->
    call(pick(Key), {add_mnesia, Key, Value}).

-spec(do_add_mnesia(dgiot_type:key()) -> ok | {error, term()}).
do_add_mnesia(Key) ->
    do_add_mnesia(Key, node()).

-spec(do_add_mnesia(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
do_add_mnesia(Key, Value) ->
    Mnesia = #mnesia{key = Key, value = Value},
    case lists:member(Mnesia, do_add_mnesia(Key)) of
        true -> ok;
        false ->
            ok = dgiot_mnesia_helper:monitor(Value),
            insert_direct_mnseia(Mnesia)
    end.

%% @doc Match mnesia
-spec(match_mnesia(dgiot_type:key()) -> [dgiot_type:mnesia()]).
match_mnesia(Key) when is_binary(Key) ->
    lookup_mnesia(Key).

-spec(lookup_mnesia(dgiot_type:key()) -> [dgiot_type:mnesia()]).
lookup_mnesia(Key) ->
    ets:lookup(?MNESIA_TAB, Key).

-spec(has_mnesia(dgiot_type:key()) -> boolean()).
has_mnesia(Key) when is_binary(Key) ->
    ets:member(?MNESIA_TAB, Key).

-spec(delete_mnesia(dgiot_type:key()) -> ok | {error, term()}).
delete_mnesia(Key) when is_binary(Key) ->
    delete_mnesia(Key, node()).

-spec(delete_mnesia(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
delete_mnesia(Key, Value) when is_binary(Key) ->
    call(pick(Key), {delete_mnesia, Key, Value}).

-spec(do_delete_mnesia(dgiot_type:key()) -> ok | {error, term()}).
do_delete_mnesia(Key) when is_binary(Key) ->
    do_delete_mnesia(Key, node()).

-spec(do_delete_mnesia(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
do_delete_mnesia(Key, Value) ->
    Mnesia = #mnesia{key = Key, value = Value},
    delete_direct_mnesia(Mnesia).

-spec(keys() -> list(binary())).
keys() ->
    mnesia:dirty_all_keys(?MNESIA_TAB).

%% @doc Print mnesia to a key
-spec(print_mnesia(dgiot_type:key()) -> ok).
print_mnesia(Key) ->
    lists:foreach(fun(#mnesia{key = Key1, value = Value}) ->
        io:format("~p -> ~p~n", [Key1, Value])
                  end, match_mnesia(Key)).

call(Mnesia, Msg) ->
    gen_server:call(Mnesia, Msg, infinity).

pick(Mnesia) ->
    gproc_pool:pick_worker(mnesia_pool, Mnesia).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Pool, Id]) ->
    true = gproc_pool:connect_worker(Pool, {Pool, Id}),
    {ok, #{pool => Pool, id => Id}}.

handle_call({add_mnesia, Key, Value}, _From, State) ->
    Ok = do_add_mnesia(Key, Value),
    {reply, Ok, State};

handle_call({delete_mnesia, Key, Value}, _From, State) ->
    Ok = do_delete_mnesia(Key, Value),
    {reply, Ok, State};

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Req]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #{pool := Pool, id := Id}) ->
    gproc_pool:disconnect_worker(Pool, {Pool, Id}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

insert_direct_mnseia(Mnesia) ->
    mnesia:async_dirty(fun mnesia:write/3, [?MNESIA_TAB, Mnesia, sticky_write]).

delete_direct_mnesia(Mnesia) ->
    mnesia:async_dirty(fun mnesia:delete_object/3, [?MNESIA_TAB, Mnesia, sticky_write]).

%% @private
-spec(maybe_trans(function(), list(any())) -> ok | {error, term()}).
maybe_trans(Fun, Args) ->
    case persistent_term:get(dgiot_mnesia_lock_type) of
        key ->
            trans(Fun, Args);
        global ->
            lock_mnesia(),
            try mnesia:sync_dirty(Fun, Args)
            after
                unlock_mnesia()
            end;
        _ ->
            pass
    end.

%% The created fun only terminates with explicit exception
-dialyzer({nowarn_function, [trans/2]}).

-spec(trans(function(), list(any())) -> ok | {error, term()}).
trans(Fun, Args) ->
    {WPid, RefMon} =
        spawn_monitor(
            %% NOTE: this is under the assumption that crashes in Fun
            %% are caught by mnesia:transaction/2.
            %% Future changes should keep in mind that this process
            %% always exit with database write result.
            fun() ->
                Res = case mnesia:transaction(Fun, Args) of
                          {atomic, Ok} -> Ok;
                          {aborted, Reason} -> {error, Reason}
                      end,
                exit({shutdown, Res})
            end),
    %% Receive a 'shutdown' exit to pass result from the short-lived process.
    %% so the receive below can be receive-mark optimized by the compiler.
    %%
    %% If the result is sent as a regular message, we'll have to
    %% either demonitor (with flush which is essentially a 'receive' since
    %% the process is no longer alive after the result has been received),
    %% or use a plain 'receive' to drain the normal 'DOWN' message.
    %% However the compiler does not optimize this second 'receive'.
    receive
        {'DOWN', RefMon, process, WPid, Info} ->
            case Info of
                {shutdown, Result} -> Result;
                _ -> {error, {trans_crash, Info}}
            end
    end.

lock_mnesia() ->
    %% if Retry is not 0, global:set_lock could sleep a random time up to 8s.
    %% Considering we have a limited number of brokers, it is safe to use sleep 1 ms.
    case global:set_lock({?MODULE, self()}, [node() | nodes()], 0) of
        false ->
            %% Force to sleep 1ms instead.
            timer:sleep(1),
            lock_mnesia();
        true ->
            ok
    end.

unlock_mnesia() ->
    global:del_lock({?MODULE, self()}).
