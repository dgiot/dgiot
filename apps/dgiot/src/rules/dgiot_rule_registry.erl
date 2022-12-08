%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_rule_registry).

-behaviour(gen_server).

-include_lib("dgiot/include/rule_engine.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("stdlib/include/qlc.hrl").
-define(DGIOT_RULE_TAB, dgiot_rule).
-define(DG_KV_TAB, '@dg_rule_engine_db').
-export([start_link/0]).

%% Rule Management
-export([ get_rules/0
        , get_rules_for/1
        , get_rules_with_same_event/1
        , get_rules_ordered_by_ts/0
        , get_rule/1
        , add_rule/1
        , add_rules/1
        , remove_rule/1
        , remove_rules/1
        ]).

-export([ load_hooks_for_rule/1
        , unload_hooks_for_rule/1
        ]).

%% for debug purposes
-export([dump/0]).

%% gen_server Callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Mnesia bootstrap
-export([mnesia/1]).

-boot_mnesia({mnesia, [boot]}).
-copy_mnesia({mnesia, [copy]}).

-define(REGISTRY, ?MODULE).

-define(T_CALL, 10000).

%%------------------------------------------------------------------------------
%% Mnesia bootstrap
%%------------------------------------------------------------------------------

%% @doc Create or replicate tables.
-spec(mnesia(boot | copy) -> ok).
mnesia(boot) ->
    %% Optimize storage
    StoreProps = [{ets, [{read_concurrency, true}]}],
    %% Rule table
    ok = ekka_mnesia:create_table(?DGIOT_RULE_TAB, [
                {ram_copies, [node()]},
                {record_name, rule},
                {index, [#rule.for]},
                {attributes, record_info(fields, rule)},
                {storage_properties, StoreProps}]);

mnesia(copy) ->
    %% Copy rule table
    ok = ekka_mnesia:copy_table(?DGIOT_RULE_TAB, ram_copies).

dump() ->
    io:format("Rules: ~p~n",
            [ets:tab2list(?DGIOT_RULE_TAB)]).

%%------------------------------------------------------------------------------
%% Start the registry
%%------------------------------------------------------------------------------

-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?REGISTRY}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% Rule Management
%%------------------------------------------------------------------------------

-spec(get_rules() -> list(emqx_rule_engine:rule())).
get_rules() ->
    get_all_records(?DGIOT_RULE_TAB).

get_rules_ordered_by_ts() ->
    F = fun() ->
        Query = qlc:q([E || E <- mnesia:table(?DGIOT_RULE_TAB)]),
        qlc:e(qlc:keysort(#rule.created_at, Query, [{order, ascending}]))
    end,
    {atomic, List} = mnesia:transaction(F),
    List.

-spec(get_rules_for(Topic :: binary()) -> list(emqx_rule_engine:rule())).
get_rules_for(Topic) ->
    [Rule || Rule = #rule{for = For} <- get_rules(),
             emqx_rule_utils:can_topic_match_oneof(Topic, For)].

-spec(get_rules_with_same_event(Topic :: binary()) -> list(emqx_rule_engine:rule())).
get_rules_with_same_event(Topic) ->
    EventName = emqx_rule_events:event_name(Topic),
    [Rule || Rule = #rule{for = For} <- get_rules(),
             lists:any(fun(T) -> is_of_event_name(EventName, T) end, For)].

is_of_event_name(EventName, Topic) ->
    EventName =:= emqx_rule_events:event_name(Topic).

-spec(get_rule(Id :: rule_id()) -> {ok, emqx_rule_engine:rule()} | not_found).
get_rule(Id) ->
    case mnesia:dirty_read(?RULE_TAB, Id) of
        [Rule] -> {ok, Rule};
        [] -> not_found
    end.

-spec(add_rule(emqx_rule_engine:rule()) -> ok).
add_rule(Rule) when is_record(Rule, rule) ->
    add_rules([Rule]).

-spec(add_rules(list(emqx_rule_engine:rule())) -> ok).
add_rules(Rules) ->
    gen_server:call(?REGISTRY, {add_rules, Rules}, ?T_CALL).

-spec(remove_rule(emqx_rule_engine:rule() | rule_id()) -> ok).
remove_rule(RuleOrId) ->
    remove_rules([RuleOrId]).

-spec(remove_rules(list(emqx_rule_engine:rule()) | list(rule_id())) -> ok).
remove_rules(Rules) ->
    gen_server:call(?REGISTRY, {remove_rules, Rules}, ?T_CALL).

%% @private
insert_rule(Rule) ->
    _ = ?CLUSTER_CALL(load_hooks_for_rule, [Rule]),
    mnesia:write(?DGIOT_RULE_TAB, Rule, write).

%% @private
delete_rule(RuleId) when is_binary(RuleId) ->
    case get_rule(RuleId) of
        {ok, Rule} -> delete_rule(Rule);
        not_found -> ok
    end;
delete_rule(Rule) ->
    _ = ?CLUSTER_CALL(unload_hooks_for_rule, [Rule]),
    mnesia:delete_object(?DGIOT_RULE_TAB, Rule, write).

load_hooks_for_rule(#rule{for = Topics}) ->
    lists:foreach(fun emqx_rule_events:load/1, Topics).

unload_hooks_for_rule(#rule{id = Id, for = Topics}) ->
    lists:foreach(fun(Topic) ->
            case get_rules_with_same_event(Topic) of
                [#rule{id = Id}] -> %% we are now deleting the last rule
                    emqx_rule_events:unload(Topic);
                _ -> ok
            end
        end, Topics).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    _TableId = ets:new(?DG_KV_TAB, [named_table, set, public, {write_concurrency, true},
                                 {read_concurrency, true}]),
    {ok, #{}}.

handle_call({add_rules, Rules}, _From, State) ->
    trans(fun lists:foreach/2, [fun insert_rule/1, Rules]),
    {reply, ok, State};

handle_call({remove_rules, Rules}, _From, State) ->
    trans(fun lists:foreach/2, [fun delete_rule/1, Rules]),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    ?LOG(error, "[RuleRegistry]: unexpected call - ~p", [Req]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    ?LOG(error, "[RuleRegistry]: unexpected cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG(error, "[RuleRegistry]: unexpected info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------

get_all_records(Tab) ->
    %mnesia:dirty_match_object(Tab, mnesia:table_info(Tab, wild_pattern)).
    ets:tab2list(Tab).

trans(Fun, Args) ->
    case mnesia:transaction(Fun, Args) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> error(Reason)
    end.
