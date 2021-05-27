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
    page/6,
    lookup/2,
    insert/2,
    delete/2,
    select_count/1,
    select_where/2,
    select_where/3,
    select_limit/2,
    select_object/3,
    match_object/2
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

-define(MNESIA_TAB, dgiot_mnesia).

%%--------------------------------------------------------------------
%% Mnesia bootstrap
%%--------------------------------------------------------------------

mnesia(boot) ->
    ok = ekka_mnesia:create_table(?MNESIA_TAB, [
        {type, bag},
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
    gen_server:start_link({local, emqx_misc:proc_name(?MODULE, Id)},
        ?MODULE, [Pool, Id], [{hibernate_after, 1000}]).

%%--------------------------------------------------------------------
%% Mnesia APIs
%%--------------------------------------------------------------------
lookup(Tab, Key) ->
    mnesia:transaction(fun mnesia:read/1, [{Tab, Key}]).

insert(TAB, Record) ->
    F = fun() -> mnesia:write(TAB, Record, write) end,
    mnesia:transaction(F).

delete(TAB, Key) ->
    F =
        fun() ->
            mnesia:delete({TAB, Key})
        end,
    mnesia:transaction(F).


select_count(TAB) ->
    F =
        fun() ->
            mnesia:table_info(TAB, size)
        end,
    mnesia:transaction(F).


select_limit(TAB, Limit) ->
    F =
        fun() ->
            Q = qlc:q([E || E <- mnesia:table(TAB)]),
            QC = qlc:cursor(Q),
            qlc:next_answers(QC, Limit)
        end,
    mnesia:transaction(F).


match_object(TB, Record) ->
    F =
        fun() ->
            mnesia:match_object(TB, Record, read)
        end,
    mnesia:transaction(F).


%% where qlc:q([E || E <- mnesia:table(TAB), E#tabrecord.id > 5])
%% order fun(A, B) -> B#tabrecord.id > A#tabrecord.id end,
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
    mnesia:transaction(F).


select_object(TB, Filter, Order) ->
    F =
        fun() ->
            shuwa_pager:all(mnesia:table(TB), Filter, Order)
        end,
    mnesia:transaction(F).


page(TB, PageNo, PageSize, Filter, RowFun) ->
    page(TB, PageNo, PageSize, Filter, RowFun, none).
page(TB, PageNo, PageSize, Filter, RowFun, Order) ->
    F =
        fun() ->
            dgiot_pager:page(mnesia:table(TB), Filter, PageNo, PageSize, RowFun, Order)
        end,
    mnesia:transaction(F).

-spec(add_mnesia(binary()) -> ok | {error, term()}).
add_mnesia(Topic) when is_binary(Topic) ->
    add_mnesia(Topic, node()).

-spec(add_mnesia(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
add_mnesia(Topic, Dest) when is_binary(Topic) ->
    call(pick(Topic), {add_mnesia, Topic, Dest}).

-spec(do_add_mnesia(dgiot_type:key()) -> ok | {error, term()}).
do_add_mnesia(Topic) when is_binary(Topic) ->
    do_add_mnesia(Topic, node()).

-spec(do_add_mnesia(dgiot_type:key(), dgiot_type:value()) -> ok | {error, term()}).
do_add_mnesia(Key, Value) when is_binary(Key) ->
    Mnesia = #mnesia{key = Key, value = Value},
    case lists:member(Mnesia, do_add_mnesia(Key)) of
        true -> ok;
        false ->
            ok = dgiot_mnseia_helper:monitor(Value),
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
        io:format("~s -> ~s~n", [Key1, Value])
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
