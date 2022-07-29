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

-module(dgiot_rule_engine).

-include_lib("emqx_rule_engine/include/rule_engine.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([create_rule/1
    , update_rule/1
    , delete_rule/1
    , refresh_rules/0
    , test/0
]).

-type(rule() :: #rule{}).

-export_type([rule/0
]).

-define(T_RETRY, 60000).

%% APIs for rules and resources
%%------------------------------------------------------------------------------

-dialyzer([{nowarn_function, [create_rule/1, rule_id/0]}]).
-spec create_rule(map()) -> {ok, rule()} | {error, term()}.
create_rule(Params = #{rawsql := Sql}) ->
    case dgiot_rule_sqlparser:parse_select(Sql) of
        {ok, Select} ->
            RuleId = maps:get(id, Params, rule_id()),
            Enabled = maps:get(enabled, Params, true),
            Rule = #rule{
                id = RuleId,
                rawsql = Sql,
                for = dgiot_rule_sqlparser:select_from(Select),
                is_foreach = dgiot_rule_sqlparser:select_is_foreach(Select),
                fields = dgiot_rule_sqlparser:select_fields(Select),
                doeach = dgiot_rule_sqlparser:select_doeach(Select),
                incase = dgiot_rule_sqlparser:select_incase(Select),
                conditions = dgiot_rule_sqlparser:select_where(Select),
                enabled = Enabled,
                created_at = erlang:system_time(millisecond),
                description = maps:get(description, Params, ""),
                state = normal
            },
            ok = dgiot_rule_registry:add_rule(Rule),
            {ok, Rule};
        Reason -> {error, Reason}
    end.

-spec(update_rule(#{id := binary(), _ => _}) -> {ok, rule()} | {error, {not_found, rule_id()}}).
update_rule(Params = #{id := RuleId}) ->
    case dgiot_rule_registry:get_rule(RuleId) of
        {ok, Rule0} ->
            try may_update_rule_params(Rule0, Params) of
                Rule ->
                    ok = dgiot_rule_registry:add_rule(Rule),
                    {ok, Rule}
            catch
                throw:Reason ->
                    {error, Reason}
            end;
        not_found ->
            {error, {not_found, RuleId}}
    end.

-spec(delete_rule(RuleId :: rule_id()) -> ok).
delete_rule(RuleId) ->
    case dgiot_rule_registry:get_rule(RuleId) of
        {ok, Rule} ->
            try
                _ = ?CLUSTER_CALL(clear_rule, [Rule]),
                ok = dgiot_rule_registry:remove_rule(Rule)
            catch
                Error:Reason:ST ->
                    ?LOG(error, "clear_rule ~p failed: ~p", [RuleId, {Error, Reason, ST}])
            end;
        not_found ->
            ok
    end.

-spec(refresh_rules() -> ok).
refresh_rules() ->
    lists:foreach(fun
                      (#rule{enabled = true} = Rule) ->
                          try refresh_rule(Rule)
                          catch _:_ ->
                              dgiot_rule_registry:add_rule(Rule#rule{enabled = false, state = refresh_failed_at_bootup})
                          end;
                      (_) -> ok
                  end, dgiot_rule_registry:get_rules()).

refresh_rule(#rule{id = RuleId, for = Topics}) ->
    ok = emqx_rule_metrics:create_rule_metrics(RuleId),
    lists:foreach(fun emqx_rule_events:load/1, Topics).

-dialyzer([{nowarn_function, may_update_rule_params/2}]).
may_update_rule_params(Rule, Params = #{rawsql := SQL}) ->
    case emqx_rule_sqlparser:parse_select(SQL) of
        {ok, Select} ->
            may_update_rule_params(
                Rule#rule{
                    rawsql = SQL,
                    for = dgiot_rule_sqlparser:select_from(Select),
                    is_foreach = dgiot_rule_sqlparser:select_is_foreach(Select),
                    fields = dgiot_rule_sqlparser:select_fields(Select),
                    doeach = dgiot_rule_sqlparser:select_doeach(Select),
                    incase = dgiot_rule_sqlparser:select_incase(Select),
                    conditions = dgiot_rule_sqlparser:select_where(Select)
                },
                maps:remove(rawsql, Params));
        Reason -> throw(Reason)
    end;
may_update_rule_params(Rule = #rule{enabled = OldEnb, actions = Actions, state = OldState},
        Params = #{enabled := NewEnb}) ->
    State = case {OldEnb, NewEnb} of
                {false, true} ->
                    _ = ?CLUSTER_CALL(refresh_rule, [Rule]),
                    force_changed;
                {true, false} ->
                    _ = ?CLUSTER_CALL(clear_actions, [Actions]),
                    force_changed;
                _NoChange -> OldState
            end,
    may_update_rule_params(Rule#rule{enabled = NewEnb, state = State}, maps:remove(enabled, Params));
may_update_rule_params(Rule, Params = #{description := Descr}) ->
    may_update_rule_params(Rule#rule{description = Descr}, maps:remove(description, Params));
may_update_rule_params(Rule, _Params) -> %% ignore all the unsupported params
    Rule.

rule_id() ->
    gen_id("rule:", fun dgiot_rule_registry:get_rule/1).

gen_id(Prefix, TestFun) ->
    Id = iolist_to_binary([Prefix, emqx_rule_id:gen()]),
    case TestFun(Id) of
        not_found -> Id;
        _Res -> gen_id(Prefix, TestFun)
    end.

test() ->
    Sql  = <<"SELECT "
    "  case "
    "     when a = 1 then a "
    "     when a = 2 then a "
    "     else a-1 "
    "  end "
    "FROM abc">>,
    dgiot_rule_sqlparser:parse_select(Sql),
    create_rule(#{rawsql => Sql}).
