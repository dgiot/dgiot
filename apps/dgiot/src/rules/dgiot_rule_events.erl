%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd.
%%--------------------------------------------------------------------

-module(dgiot_rule_events).
-author("dgiot").

-export([load/1, unload/1, event_name/1]).

load(Topic) ->
    dgiot_hooks:add(event_name(Topic), {?MODULE, on_event, [Topic]}).

unload(Topic) ->
    dgiot_hooks:del(event_name(Topic), {?MODULE, on_event, [Topic]}).

event_name(Topic) when is_binary(Topic) ->
    list_to_atom("$event/" ++ binary_to_list(Topic));
event_name(Topic) when is_list(Topic) ->
    list_to_atom("$event/" ++ Topic).

on_event(_Topic) -> ok.
