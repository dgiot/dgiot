%% @doc 同名承接：emqx_topic（影子内核，刀 4）。
%% 主题工具函数转发到我们的 router（匹配/校验逻辑同源，避免两套语义）。
-module(emqx_topic).

-export([match/2, validate/1, wildcard/1, join/1, split/1, split/2,
         words/1, is_valid/1]).

match(Filter, Topic) ->
    dgiot_broker_router:topic_matches(Filter, Topic).

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
