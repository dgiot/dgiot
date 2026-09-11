%% @doc 同名承接：emqx_broker（刀 4/5 手写实现）。
%% dgiot 的 channels/bridge/task 用它做 VM 内发布订阅；消息为
%% #message{} 记录（EMQX 语义，见 emqx_message）。
%% 未实现函数显式报错（不静默）。
-module(emqx_broker).

-include("emqx.hrl").

-export([publish/1, safe_publish/1,
         subscribe/2, subscribe/3, unsubscribe/1,
         subscribers/1, stats/0]).

publish(#message{} = Message) ->
    Topic = emqx_message:topic(Message),
    Payload = emqx_message:payload(Message),
    %% 与 EMQX 语义一致：message.publish 钩子在投递前可见
    _ = emqx_hooks:run('message.publish', [Message]),
    emqx_metrics:inc('messages.publish'),
    case dgiot_broker_native:publish(emqx_message:from(Message),
                                     Topic, Payload) of
        {ok, Delivered} ->
            [emqx_metrics:inc('messages.delivered') || _ <- lists:seq(1, Delivered)],
            {ok, Delivered};
        {error, Reason} ->
            {error, Reason}
    end;
publish(Other) ->
    {error, {bad_message, Other}}.

%% 语义同 EMQX：safe_publish 失败不抛，返回 {error, _}
safe_publish(Message) ->
    try publish(Message)
    catch Class:Reason ->
        {error, {Class, Reason}}
    end.

subscribe(Pid, Filter) -> subscribe(Pid, Filter, 0).
%% VM 内订阅者按 EMQX 语义收记录（shape=record → {deliver, #message{}}）
subscribe(Pid, Filter, Qos) ->
    dgiot_broker_router:subscribe(Filter, client_id(Pid), Pid, Qos,
                                  #{shape => record}).

unsubscribe(Pid) ->
    dgiot_broker_router:unsubscribe_all(client_id(Pid)),
    ok.

subscribers(Topic) ->
    [Pid || {_Cid, Pid, _Q} <- dgiot_broker_router:match(Topic)].

stats() ->
    #{subscriptions => dgiot_broker_router:count(),
      sessions => dgiot_broker_session:count()}.

client_id(Pid) -> iolist_to_binary(io_lib:format("inproc-~p", [Pid])).
