%% @doc 同名承接：emqx_shared_sub（共享订阅）→ 基础实现（单组，先到先得）。
%% 完整 $share/group 语义属刀 7；这里至少让 subscribe/unsubscribe 可用，
%% 并显式标注未实现的分组语义。
-module(emqx_shared_sub).

-export([subscribe/3, unsubscribe/3, members/1, groups/0]).

subscribe(Group, Topic, SubOpts) when is_binary(Group), is_binary(Topic) ->
    Pid = maps:get(subpid, SubOpts, self()),
    %% 简化：当作普通订阅登记（分组成员 = 全部订阅者）
    dgiot_broker_router:subscribe(Topic,
                                  iolist_to_binary(io_lib:format("~s-~p",
                                                                 [Group, Pid])),
                                  Pid, maps:get(qos, SubOpts, 0)),
    ok.

unsubscribe(Group, Topic, _SubOpts) when is_binary(Group) ->
    Pid = self(),
    dgiot_broker_router:unsubscribe(
      Topic, iolist_to_binary(io_lib:format("~s-~p", [Group, Pid]))),
    ok.

members(Group) ->
    [C || {{_F, C}, _V} <- dgiot_broker_router:subscriptions(),
          binary:match(C, Group) =/= nomatch].

groups() ->
    [].
