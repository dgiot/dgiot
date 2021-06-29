%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 九月 2019 11:30
%%%-------------------------------------------------------------------
-module(shuwa_mqtt).
-author("jonhliu").
-include("shuwa_mqtt.hrl").
-compile(export_all).

%% API
-export([start_mqtt_workers/1]).

start_mqtt_workers(#{<<"threshold">> := Threshold,
  <<"tid">> := Tid,
  <<"di">> := Di,
  <<"mod">> := Mod,
  <<"fun">> := Fun}) ->
  shuwa_data:set_consumer(?COMSUMER_KEY(Di), Threshold),
  lists:foreach(
    fun(I) ->
      Args = #{<<"tid">> => Tid,
        <<"mid">> => I,
        <<"di">> => Di,
        <<"mod">> => Mod,
        <<"fun">> => Fun},
      supervisor:start_child(group_mqtt, [Args])
    end, lists:seq(1, Threshold)).

has_routes(Topic) ->
  emqx_router:has_routes(Topic).

subscribe(Topic) ->
  Options = #{qos => 0},
  timer:sleep(1),
  emqx:subscribe(Topic, shuwa_utils:to_binary(self()), Options).

unsubscribe(Topic) ->
  emqx_broker:unsubscribe(iolist_to_binary(Topic)).

-spec(publish(Client :: binary(), Topic :: binary(), Payload :: binary())
      -> ok | {error, Reason :: any()}).
publish(Client, Topic, Payload) ->
  timer:sleep(10),
  Msg = emqx_message:make(shuwa_utils:to_binary(Client), 0, Topic, Payload),
  emqx:publish(Msg),
  ok.

publish(Client, Topic, Payload, check_route) ->
  case emqx_router:has_routes(Topic) of
    true ->
      publish(Client, Topic, Payload);
    false -> ok
  end;

publish(Client, Di, Payload, comsumer) ->
  Mid = shuwa_data:get_consumer(?COMSUMER_KEY(Di), 1),
  BinMid = shuwa_utils:to_binary(Mid),
  publish(Client, ?GROUP_TOPIC(Di, BinMid), Payload);

publish(Client, Topic, Payload, _) ->
  publish(Client, Topic, Payload).

shared_sub(Group, Topic, SubPid) ->
  emqx_shared_sub:subscribe(Group, Topic, SubPid).

shared_unsub(Group, Topic, SubPid) ->
  emqx_shared_sub:unsubscribe(Group, Topic, SubPid).

add_hook(HookPoint, Callback) ->
  emqx_hooks:add(HookPoint, Callback).

add_hook(HookPoint, Action, Args) ->
  emqx_hooks:add(HookPoint, Action, Args).

add_hook(HookPoint, Action, Filter, Priority) ->
  emqx_hooks:add(HookPoint, Action, Filter, Priority).

del_hook(HookPoint, Action) ->
  emqx_hooks:del(HookPoint, Action).

run_hook(HookPoint, Args) ->
  emqx_hooks:run(HookPoint, Args).

run_hook_fold(HookPoint, Args, Acc) ->
  emqx_hooks:run_fold(HookPoint, Args, Acc).

get_payload(Msg) ->
  Msg#message.payload.

get_topic(Msg) ->
  Msg#message.topic.