%% @doc 订阅路由簿（刀 3）：注册订阅 + 主题匹配。
%%
%% 刀 3 只做"注册与可观测"；投递在刀 4。匹配按 MQTT 3.1.1 规则：
%%   * '+' 匹配恰好一层；'#' 匹配剩余所有层（只能出现在末尾）
%%   * 首层为 '$' 的主题（$SYS/$dg）不被通配订阅匹配（协议规定）
%%   * 订阅过滤器本身非法 → 显式 {error, Reason}，绝不"存下来以后再说"
%%
%% 实现：ETS 集合表 {Filter, ClientId} → #{pid, qos}；匹配按过滤器遍历。
%% 规模上限：实验室级（数百订阅）足够；大规模需要 trie，留作后续刀，
%% 此处明确标注而不是假装它能扛百万连接。
-module(dgiot_broker_router).

-export([init/0, reset/0,
         subscribe/4, unsubscribe/2, unsubscribe_all/1,
         match/1, subscriptions/0, count/0,
         validate_filter/1, topic_matches/2]).

-define(TAB, dgiot_broker_routes).

init() ->
    case ets:info(?TAB) of
        undefined ->
            ets:new(?TAB, [named_table, set, public,
                           {read_concurrency, true}]);
        _ ->
            ?TAB
    end,
    ok.

reset() ->
    ets:delete_all_objects(?TAB),
    ok.

%% @doc 注册订阅。过滤器非法时显式报错（由调用方回 SUBACK 0x80）。
-spec subscribe(binary(), binary(), pid(), 0 | 1 | 2) -> ok | {error, term()}.
subscribe(Filter, ClientId, Pid, Qos0) ->
    case validate_filter(Filter) of
        ok when Qos0 >= 0, Qos0 =< 2 ->
            ets:insert(?TAB, {{Filter, ClientId}, #{pid => Pid, qos => Qos0,
                                                    ts => erlang:system_time(second)}}),
            ok;
        ok ->
            {error, {bad_qos, Qos0}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec unsubscribe(binary(), binary()) -> ok.
unsubscribe(Filter, ClientId) ->
    ets:delete(?TAB, {Filter, ClientId}),
    ok.

-spec unsubscribe_all(binary()) -> non_neg_integer().
unsubscribe_all(ClientId) ->
    Keys = [K || {{_F, C} = K, _V} <- ets:tab2list(?TAB), C =:= ClientId],
    [ets:delete(?TAB, K) || K <- Keys],
    length(Keys).

%% @doc 主题 → 命中的订阅列表（刀 4 投递用；刀 3 用于可观测与自检）
-spec match(binary()) -> [{binary(), pid(), 0 | 1 | 2}].
match(Topic) ->
    lists:foldl(
      fun({{Filter, ClientId}, #{pid := Pid, qos := Qos}}, Acc) ->
              case topic_matches(Filter, Topic) of
                  true -> [{ClientId, Pid, Qos} | Acc];
                  false -> Acc
              end
      end, [], ets:tab2list(?TAB)).

subscriptions() ->
    [{F, C, V} || {{F, C}, V} <- ets:tab2list(?TAB)].

count() ->
    ets:info(?TAB, size).

%% @doc 过滤器合法性（MQTT 3.1.1 §4.7）
validate_filter(<<>>) ->
    {error, empty_filter};
validate_filter(Filter) when is_binary(Filter) ->
    Levels = binary:split(Filter, <<"/">>, [global]),
    validate_levels(Levels, 1, length(Levels));
validate_filter(Other) ->
    {error, {not_binary, Other}}.

validate_levels([], _N, _Total) -> ok;
validate_levels([<<"#">>], N, Total) when N =:= Total -> ok;
validate_levels([<<"#">> | _], _N, _Total) -> {error, hash_not_last};
validate_levels([Level | Rest], N, Total) ->
    case binary:match(Level, <<"#">>) of
        nomatch ->
            case binary:match(Level, <<"+">>) of
                nomatch -> validate_levels(Rest, N + 1, Total);
                _ when Level =:= <<"+">> -> validate_levels(Rest, N + 1, Total);
                _ -> {error, {plus_in_level, Level}}
            end;
        _ ->
            {error, {hash_in_level, Level}}
    end.

%% @doc MQTT 主题匹配（含 $ 前缀保护）
-spec topic_matches(binary(), binary()) -> boolean().
topic_matches(Filter, Topic) ->
    case {match_filter_levels(Filter, Topic),
          should_protect_dollar(Filter, Topic)} of
        {true, true} -> false;
        {true, false} -> true;
        _ -> false
    end.

%% 通配订阅不得命中 $-开头主题的首层
should_protect_dollar(<<"$", _/binary>>, _Topic) -> false;   %% 显式订阅 $ 主题 → 允许
should_protect_dollar(_Filter, <<"$", _/binary>>) -> true;
should_protect_dollar(_F, _T) -> false.

match_filter_levels(Filter, Topic) ->
    match_levels(binary:split(Filter, <<"/">>, [global]),
                 binary:split(Topic, <<"/">>, [global])).

match_levels([<<"#">>], _TopicLevels) -> true;
match_levels([], []) -> true;
match_levels([], _) -> false;
match_levels(_, []) -> false;
match_levels([<<"+">> | FR], [_ | TR]) -> match_levels(FR, TR);
match_levels([L | FR], [L | TR]) -> match_levels(FR, TR);
match_levels(_, _) -> false.
