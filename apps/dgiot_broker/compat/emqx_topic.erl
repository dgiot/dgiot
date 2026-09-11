%% @doc 同名承接：emqx_topic（影子内核，刀 4）。
%% 主题工具函数转发到我们的 router（匹配/校验逻辑同源，避免两套语义）。
-module(emqx_topic).

-export([match/2, validate/1, validate/2, wildcard/1, join/1, split/1, split/2,
         words/1, tokens/1, prepend/2, parse/1, parse/2, is_valid/1,
         topic/0]).

-export_type([topic/0]).
-type topic() :: binary().

%% 插件里用 emqx_topic:topic() 作类型/默认值
topic() -> <<>>.

match(Filter, Topic) ->
    dgiot_broker_router:topic_matches(Filter, Topic).

%% parse/1：主题 → 层级列表（EMQX 语义）；非法主题返回 {error, _}
parse(Topic) when is_binary(Topic) ->
    case dgiot_broker_router:validate_filter(Topic) of
        ok -> binary:split(Topic, <<"/">>, [global]);
        {error, Reason} -> {error, Reason}
    end;
parse(Other) -> {error, {bad_topic, Other}}.

%% parse/2：带分隔符（EMQX 允许自定义）
parse(Topic, Sep) when is_binary(Topic), is_binary(Sep) ->
    binary:split(Topic, Sep, [global]);
parse(Other, _Sep) -> {error, {bad_topic, Other}}.

tokens(Topic) when is_binary(Topic) ->
    binary:split(Topic, <<"/">>, [global]);
tokens(Other) -> {error, {bad_topic, Other}}.

%% prepend/2：给主题加前缀（EMQX 的 mountpoint 语义）
prepend(<<>>, Topic) -> Topic;
prepend(Prefix, Topic) when is_binary(Prefix), is_binary(Topic) ->
    <<Prefix/binary, Topic/binary>>.

validate(Filter) ->
    case dgiot_broker_router:validate_filter(Filter) of
        ok -> true;
        {error, _} -> false
    end.

is_valid(Filter) -> validate(Filter).

wildcard(<<"#">>) -> true;
wildcard(<<"+">>) -> true;
wildcard(Topic) when is_binary(Topic) ->
    lists:any(fun(L) -> L =:= <<"#">> orelse L =:= <<"+">> end,
              binary:split(Topic, <<"/">>, [global])).

join(Words) when is_list(Words) -> iolist_to_binary(lists:join(<<"/">>, Words));
join(Bin) when is_binary(Bin) -> Bin.

split(Topic) -> split(Topic, <<"/">>).
split(Topic, Sep) when is_binary(Topic) ->
    binary:split(Topic, Sep, [global]).

words(Topic) -> split(Topic).

%% validate/2：EMQX 允许关闭通配检查（严格的 ACL 校验用）
validate(Topic, _StrictOpts) -> validate(Topic).
