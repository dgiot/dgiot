%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 九月 2019 17:00
%%%-------------------------------------------------------------------
-module(shuwa_mqtt_hooks).
-author("kenneth").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx_rule_engine/include/rule_engine.hrl").
-export([start/1, stop/1]).

-export([
    on_client_connected/4,
    on_client_disconnected/4,
    on_client_subscribe/4,
    on_client_unsubscribe/4,
    on_message_publish/2,
    on_message_dropped/3,
    on_message_deliver/3,
    on_message_acked/3,
    on_session_subscribed/4,
    on_session_unsubscribed/4
]).

%%------------------------------------------------------------------------------
%% Start
%%------------------------------------------------------------------------------

start(Env) ->
    add_actions(),
    hook_rules('client.connected', fun ?MODULE:on_client_connected/4, Env),
    hook_rules('client.disconnected', fun ?MODULE:on_client_disconnected/4, Env),
    hook_rules('client.subscribe', fun ?MODULE:on_client_subscribe/4, Env),
    hook_rules('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, Env),
    hook_rules('message.publish', fun ?MODULE:on_message_publish/2, Env),
    hook_rules('message.dropped', fun ?MODULE:on_message_dropped/3, Env),
    hook_rules('message.delivered', fun ?MODULE:on_message_deliver/3, Env),
    hook_rules('message.acked', fun ?MODULE:on_message_acked/3, Env),
    hook_rules('session.subscribed', fun ?MODULE:on_session_subscribed/4, Env),
    hook_rules('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, Env),
    ok.

%%------------------------------------------------------------------------------
%% Stop
%%------------------------------------------------------------------------------

%% Called when the rule engine application stop
stop(_Env) ->
    emqx:unhook('client.connected', fun ?MODULE:on_client_connected/4),
    emqx:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/4),
    emqx:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/4),
    emqx:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4),
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    emqx:unhook('message.dropped', fun ?MODULE:on_message_dropped/3),
    emqx:unhook('message.delivered', fun ?MODULE:on_message_deliver/3),
    emqx:unhook('message.acked', fun ?MODULE:on_message_acked/3),
    emqx:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/4),
    emqx:unhook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4),
    ok.


add_actions() ->
    emqx_rule_registry:add_resource(#resource{

    }),
    emqx_rule_registry:add_action(#action{
        name = data_resource,
        for = 'any',
        app = channel,
        types = [bridge_channel],
        module = shuwa_channelx,
        on_create = {?MODULE, on_create},
        on_destroy = {?MODULE, on_destroy},
        hidden = false,
        params_spec = #{}
    }).


hook_rules(Name, Fun, Env) ->
    emqx:hook(Name, Fun, [Env#{apply_fun => apply_rules_fun(Name)}]).

%%------------------------------------------------------------------------------
%% Callbacks
%%------------------------------------------------------------------------------

on_client_connected(ClientInfo, ConnAck, ConnInfo, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(ClientInfo, #{event => 'client.connected', connack => ConnAck, conninfo => ConnInfo, node => node()})).

on_client_disconnected(ClientInfo, ReasonCode, ConnInfo, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(ClientInfo, #{event => 'client.disconnected', reason_code => ReasonCode, conninfo => ConnInfo, node => node(), timestamp => erlang:timestamp()})).

on_client_subscribe(ClientInfo, _Properties, TopicFilters, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(ClientInfo, #{event => 'client.subscribe', topic_filters => TopicFilters, node => node(), timestamp => erlang:timestamp()})),
    {ok, TopicFilters}.

on_client_unsubscribe(ClientInfo, _Properties, TopicFilters, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(ClientInfo, #{event => 'client.unsubscribe', topic_filters => TopicFilters, node => node(), timestamp => erlang:timestamp()})),
    {ok, TopicFilters}.

on_session_subscribed(ClientInfo, Topic, SubOpts, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(ClientInfo, #{event => 'session.subscribed', topic => Topic, node => node(), timestamp => erlang:timestamp(), sub_opts => SubOpts})).

on_session_unsubscribed(ClientInfo, Topic, SubOpts, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(ClientInfo, #{event => 'session.unsubscribed', topic => Topic, node => node(), timestamp => erlang:timestamp(), sub_opts => SubOpts})).

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, #{ignore_sys_message := true}) ->
    {ok, Message};

on_message_publish(Message, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(emqx_message:to_map(Message), #{event => 'message.publish', node => node()})),
    {ok, Message}.

on_message_dropped(_, Message = #message{topic = <<"$SYS/", _/binary>>},
        #{ignore_sys_message := true}) ->
    {ok, Message};

on_message_dropped(_, Message, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(emqx_message:to_map(Message), #{event => 'message.dropped', node => node()})),
    {ok, Message}.

on_message_deliver(ClientInfo, Message, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(ClientInfo#{event => 'message.delivered', node => node()}, emqx_message:to_map(Message))),
    {ok, Message}.

on_message_acked(_ClientInfo, Message, #{apply_fun := ApplyRules}) ->
    ApplyRules(maps:merge(emqx_message:to_map(Message), #{event => 'message.acked', node => node()})),
    {ok, Message}.

%%------------------------------------------------------------------------------
%% Apply rules
%%------------------------------------------------------------------------------

apply_rules_fun(Hook) ->
    fun(Input) -> apply_rules(rules_for(Hook), Input) end.

rules_for(Hook) ->
    emqx_rule_registry:get_rules_for(Hook).

apply_rules([], _Input) ->
    clear_rule_payload(),
    ok;
apply_rules([#rule{enabled = false} | More], Input) ->
    apply_rules(More, Input);
apply_rules([Rule = #rule{id = RuleID} | More], Input) ->
    try
        apply_rule(Rule, Input)
    catch
        %% ignore the errors if select or match failed
        _:{select_and_transform_error, Error} ->
            lager:debug("SELECT clause exception for ~s failed: ~p", [RuleID, Error]);
        _:{match_conditions_error, Error} ->
            lager:debug("WHERE clause exception for ~s failed: ~p", [RuleID, Error]);
        _:{select_and_collect_error, Error} ->
            lager:debug("FOREACH clause exception for ~s failed: ~p", [RuleID, Error]);
        _:{match_incase_error, Error} ->
            lager:debug("INCASE clause exception for ~s failed: ~p", [RuleID, Error]);
        _:Error:StkTrace ->
            lager:error("Apply rule ~s failed: ~p. Stacktrace:~n~p", [RuleID, Error, StkTrace])
    end,
    apply_rules(More, Input).

apply_rule(#rule{
    id = RuleId,
    is_foreach = true,
    fields = Fields,
    doeach = DoEach,
    incase = InCase,
    conditions = Conditions,
    actions = Actions
}, Input) ->
    Columns = columns(Input),
    {Selected, Collection} = ?RAISE(select_and_collect(Fields, Columns), {select_and_collect_error, _REASON_}),
    ColumnsAndSelected = maps:merge(Columns, Selected),
    case ?RAISE(match_conditions(Conditions, ColumnsAndSelected), {match_conditions_error, _REASON_}) of
        true ->
            ok = emqx_rule_metrics:inc(RuleId, 'rules.matched'),
            Collection2 = filter_collection(Input, InCase, DoEach, Collection),
            {ok, [take_actions(Actions, Coll, Input) || Coll <- Collection2]};
        false ->
            {error, nomatch}
    end;

apply_rule(#rule{
    id = RuleId,
    is_foreach = false,
    fields = Fields,
    conditions = Conditions,
    actions = Actions
}, Input) ->
    Columns = columns(Input),
    Selected = ?RAISE(select_and_transform(Fields, Columns), {select_and_transform_error, _REASON_}),
    case ?RAISE(match_conditions(Conditions, maps:merge(Columns, Selected)), {match_conditions_error, _REASON_}) of
        true ->
            ok = emqx_rule_metrics:inc(RuleId, 'rules.matched'),
            lager:info("Rule Match ~p,~p,~p", [RuleId, Input, Actions]),
            {ok, take_actions(Actions, Selected, Input)};
        false ->
            {error, nomatch}
    end.

clear_rule_payload() ->
    erlang:erase(rule_payload).

%% SELECT Clause
select_and_transform(Fields, Input) ->
    select_and_transform(Fields, Input, #{}).

select_and_transform([], _Input, Output) ->
    Output;
select_and_transform(['*' | More], Input, Output) ->
    select_and_transform(More, Input, maps:merge(Output, Input));
select_and_transform([{as, Field, Alias} | More], Input, Output) ->
    Key = emqx_rule_utils:unsafe_atom_key(Alias),
    Val = eval(Field, Input),
    select_and_transform(More,
        nested_put(Key, Val, Input),
        nested_put(Key, Val, Output));
select_and_transform([Field | More], Input, Output) ->
    Val = eval(Field, Input),
    Key = alias(Field, Val),
    select_and_transform(More,
        nested_put(Key, Val, Input),
        nested_put(Key, Val, Output)).

%% FOREACH Clause
select_and_collect(Fields, Input) ->
    select_and_collect(Fields, Input, {#{}, {'item', []}}).

select_and_collect([{as, Field, Alias}], Input, {Output, _}) ->
    Key = emqx_rule_utils:unsafe_atom_key(Alias),
    Val = eval(Field, Input),
    {nested_put(Key, Val, Output), {Key, ensure_list(Val)}};
select_and_collect([{as, Field, Alias} | More], Input, {Output, LastKV}) ->
    Key = emqx_rule_utils:unsafe_atom_key(Alias),
    Val = eval(Field, Input),
    select_and_collect(More,
        nested_put(Key, Val, Input),
        {nested_put(Key, Val, Output), LastKV});
select_and_collect([Field], Input, {Output, _}) ->
    Val = eval(Field, Input),
    Key = alias(Field, Val),
    {nested_put(Key, Val, Output), {'item', ensure_list(Val)}};
select_and_collect([Field | More], Input, {Output, LastKV}) ->
    Val = eval(Field, Input),
    Key = alias(Field, Val),
    select_and_collect(More,
        nested_put(Key, Val, Input),
        {nested_put(Key, Val, Output), LastKV}).

%% Filter each item got from FOREACH
filter_collection(Input, InCase, DoEach, {CollKey, CollVal}) ->
    lists:filtermap(
        fun(Item) ->
            InputAndItem = maps:merge(columns(Input), #{CollKey => Item}),
            case ?RAISE(match_conditions(InCase, InputAndItem),
                {match_incase_error, _REASON_}) of
                true when DoEach == [] -> true;
                true ->
                    {true, ?RAISE(select_and_transform(DoEach, InputAndItem),
                        {doeach_error, _REASON_})};
                false -> false
            end
        end, CollVal).

%% Conditional Clauses such as WHERE, WHEN.
match_conditions({'and', L, R}, Data) ->
    match_conditions(L, Data) andalso match_conditions(R, Data);
match_conditions({'or', L, R}, Data) ->
    match_conditions(L, Data) orelse match_conditions(R, Data);
match_conditions({'not', Var}, Data) ->
    case eval(Var, Data) of
        Bool when is_boolean(Bool) ->
            not Bool;
        _other -> false
    end;
match_conditions({in, Var, {list, Vals}}, Data) ->
    lists:member(eval(Var, Data), [eval(V, Data) || V <- Vals]);
match_conditions({'fun', Name, Args}, Data) ->
    apply_func(Name, [eval(Arg, Data) || Arg <- Args], Data);
match_conditions({Op, L, R}, Data) when ?is_comp(Op) ->
    compare(Op, eval(L, Data), eval(R, Data));
%%match_conditions({'like', Var, Pattern}, Data) ->
%%    match_like(eval(Var, Data), Pattern);
match_conditions({}, _Data) ->
    true.

%% comparing numbers against strings
compare(Op, L, R) when is_number(L), is_binary(R) ->
    do_compare(Op, L, number(R));
compare(Op, L, R) when is_binary(L), is_number(R) ->
    do_compare(Op, number(L), R);
compare(Op, L, R) when is_atom(L), is_binary(R) ->
    do_compare(Op, atom_to_binary(L, utf8), R);
compare(Op, L, R) when is_binary(L), is_atom(R) ->
    do_compare(Op, L, atom_to_binary(R, utf8));
compare(Op, L, R) ->
    do_compare(Op, L, R).

do_compare('=', L, R) -> L == R;
do_compare('>', L, R) -> L > R;
do_compare('<', L, R) -> L < R;
do_compare('<=', L, R) -> L =< R;
do_compare('>=', L, R) -> L >= R;
do_compare('<>', L, R) -> L /= R;
do_compare('!=', L, R) -> L /= R;
do_compare('=~', T, F) -> emqx_topic:match(T, F).

number(Bin) ->
    try binary_to_integer(Bin)
    catch error:badarg -> binary_to_float(Bin)
    end.

%% Step3 -> Take actions
take_actions(Actions, Selected, Envs) ->
    lists:map(fun(Action) -> take_action(Action, Selected, Envs) end, Actions).

take_action(#action_instance{id = Id}, Selected, Envs) ->
    try
        {ok, #action_instance_params{apply = Apply}} = emqx_rule_registry:get_action_instance_params(Id),
        lager:info("Rule Match ~p,~p,~p", [Id, Selected, Apply]),
        Result = Apply(Selected, Envs),
        emqx_rule_metrics:inc(Id, 'actions.success'),
        Result
    catch
        _Error:Reason:Stack ->
            emqx_rule_metrics:inc(Id, 'actions.failure'),
            error({take_action_failed, {Id, Reason, Stack}})
    end.

eval({var, [<<"payload">> | Vars]}, Input) ->
    nested_get(Vars,
        case erlang:get(rule_payload) of
            undefined ->
                Map = ensure_map(nested_get(<<"payload">>, Input)),
                erlang:put(rule_payload, Map), Map;
            Map -> Map
        end);
eval({var, Var}, Input) ->
    nested_get(Var, Input);
eval({const, Val}, _Input) ->
    Val;
eval({Op, L, R}, Input) when ?is_arith(Op) ->
    apply_func(Op, [eval(L, Input), eval(R, Input)], Input);
eval({'case', undefined, CaseClauses, ElseClauses}, Input) ->
    eval_case_clauses(CaseClauses, ElseClauses, Input);
eval({'case', CaseOn, CaseClauses, ElseClauses}, Input) ->
    eval_switch_clauses(CaseOn, CaseClauses, ElseClauses, Input);
eval({'fun', Name, Args}, Input) ->
    apply_func(Name, [eval(Arg, Input) || Arg <- Args], Input).

alias(Field, Val) ->
    case alias(Field) of
        undefined -> Val;
        Alias -> Alias
    end.

alias({var, Var}) ->
    emqx_rule_utils:unsafe_atom_key(Var);
alias({const, Val}) ->
    Val;
alias(_) -> undefined.

eval_case_clauses([], ElseClauses, Input) ->
    case ElseClauses of
        undefined -> undefined;
        _ -> eval(ElseClauses, Input)
    end;
eval_case_clauses([{Cond, Clause} | CaseClauses], ElseClauses, Input) ->
    case match_conditions(Cond, Input) of
        true ->
            eval(Clause, Input);
        _ ->
            eval_case_clauses(CaseClauses, ElseClauses, Input)
    end.

eval_switch_clauses(_CaseOn, [], ElseClauses, Input) ->
    case ElseClauses of
        undefined -> undefined;
        _ -> eval(ElseClauses, Input)
    end;
eval_switch_clauses(CaseOn, [{Cond, Clause} | CaseClauses], ElseClauses, Input) ->
    ConResult = eval(Cond, Input),
    case eval(CaseOn, Input) of
        ConResult ->
            eval(Clause, Input);
        _ ->
            eval_switch_clauses(CaseOn, CaseClauses, ElseClauses, Input)
    end.

apply_func(Name, Args, Input) when is_atom(Name) ->
    case erlang:apply(emqx_rule_funcs, Name, Args) of
        Func when is_function(Func) ->
            erlang:apply(Func, [Input]);
        Result -> Result
    end.


%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

columns(Input) ->
    columns(Input, #{}).
columns(Input = #{id := Id}, Result) ->
    columns(maps:remove(id, Input),
        Result#{id => emqx_guid:to_hexstr(Id)});
columns(Input = #{from := From}, Result) ->
    columns(maps:remove(from, Input),
        Result#{clientid => From});
columns(Input = #{flags := Flags}, Result) ->
    Retain = maps:get(retain, Flags, false),
    columns(maps:remove(flags, Input),
        maps:merge(Result, #{flags => Flags,
            retain => int(Retain)}));
columns(Input = #{headers := Headers}, Result) ->
    Username = maps:get(username, Headers, undefined),
    PeerHost = host_to_str(maps:get(peerhost, Headers, undefined)),
    columns(maps:remove(headers, Input),
        maps:merge(Result, #{username => Username,
            peerhost => PeerHost}));
columns(Input = #{timestamp := Timestamp}, Result) ->
    columns(maps:remove(timestamp, Input),
        Result#{timestamp => emqx_rule_utils:now_ms(Timestamp)});
columns(Input = #{peerhost := Peername}, Result) ->
    columns(maps:remove(peerhost, Input),
        Result#{peerhost => host_to_str(Peername)});
columns(Input = #{sockname := Peername}, Result) ->
    columns(maps:remove(sockname, Input),
        Result#{sockname => host_to_str(Peername)});
columns(Input = #{conninfo := Conn}, Result) ->
    ConnAt = maps:get(connected_at, Conn, erlang:system_time(second)),
    columns(maps:remove(conninfo, Input),
        maps:merge(Result, #{connected_at => ConnAt,
            clean_start => maps:get(clean_start, Conn, undefined),
            keepalive => maps:get(keepalive, Conn, undefined),
            proto_ver => maps:get(proto_ver, Conn, undefined)
        }));
columns(Input = #{topic_filters := [{Topic, Opts} | _] = Filters}, Result) ->
    columns(maps:remove(topic_filters, Input),
        Result#{topic => Topic, qos => maps:get(qos, Opts, 0),
            topic_filters => format_topic_filters(Filters)});
columns(Input, Result) ->
    maps:merge(Result, Input).

host_to_str(undefined) ->
    undefined;
host_to_str(IPAddr) ->
    list_to_binary(inet:ntoa(IPAddr)).

int(true) -> 1;
int(false) -> 0.

format_topic_filters(Filters) ->
    [begin
         #{topic => Topic, qos => maps:get(qos, Opts, 0), sub_opts => Opts}
     end || {Topic, Opts} <- Filters].

ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(MaybeJson) ->
    try jsx:decode(MaybeJson, [return_maps]) of
        JsonMap when is_map(JsonMap) -> JsonMap;
        _Val -> #{}
    catch _:_ -> #{}
    end.

ensure_list(List) when is_list(List) -> List;
ensure_list(_NotList) -> [].




nested_get(Key, Map) ->
    nested_get(Key, Map, undefined).

nested_get(_Key, Map, Default) when not is_map(Map) ->
    Default;
nested_get(Key, Map, Default) when not is_list(Key) ->
    get_value(Key, Map, Default);
nested_get([Key], Map, Default) ->
    get_value(Key, Map, Default);
nested_get([Key | More], Map, Default) ->
    general_map(Key, Map,
        fun
            ({equivalent, {_EquiKey, Val}}) -> nested_get(More, Val, Default);
            ({found, {_Key, Val}}) -> nested_get(More, Val, Default);
            (not_found) -> Default
        end);
nested_get([], Val, _Default) ->
    Val.

nested_put(Key, Val, Map) when not is_map(Map) ->
    nested_put(Key, Val, #{});
nested_put(_, undefined, Map) ->
    Map;
nested_put(Key, Val, Map) when not is_list(Key) ->
    put_value(Key, Val, Map);
nested_put([Key], Val, Map) ->
    put_value(Key, Val, Map);
nested_put([Key | More], Val, Map) ->
    SubMap = general_map_get(Key, Map, #{}),
    put_value(Key, nested_put(More, Val, SubMap), Map);
nested_put([], Val, _Map) ->
    Val.


get_value(Key, Map, Default) ->
    general_map_get(Key, Map, Default).

put_value(_Key, undefined, Map) ->
    Map;
put_value(Key, Val, Map) ->
    general_map_put(Key, Val, Map).


general_map_get(Key, Map, Default) ->
    general_map(Key, Map,
        fun
            ({equivalent, {_EquiKey, Val}}) -> Val;
            ({found, {_Key, Val}}) -> Val;
            (not_found) -> Default
        end).

general_map_put(Key, Val, Map) ->
    general_map(Key, Map,
        fun
            ({equivalent, {EquiKey, _Val}}) -> maps:put(EquiKey, Val, Map);
            (_) -> maps:put(Key, Val, Map)
        end).

general_map(Key, Map, Handler) ->
    case maps:find(Key, Map) of
        {ok, Val} -> Handler({found, {Key, Val}});
        error when is_atom(Key) ->
            %% the map may have an equivalent binary-form key
            BinKey = emqx_rule_utils:bin(Key),
            case maps:find(BinKey, Map) of
                {ok, Val} -> Handler({equivalent, {BinKey, Val}});
                error -> Handler(not_found)
            end;
        error when is_binary(Key) ->
            try %% the map may have an equivalent atom-form key
                AtomKey = list_to_existing_atom(binary_to_list(Key)),
                case maps:find(AtomKey, Map) of
                    {ok, Val} -> Handler({equivalent, {AtomKey, Val}});
                    error -> Handler(not_found)
                end
            catch error:badarg ->
                Handler(not_found)
            end;
        error ->
            Handler(not_found)
    end.


