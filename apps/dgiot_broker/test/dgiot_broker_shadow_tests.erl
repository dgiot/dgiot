%% @doc 刀 4 影子内核行为测试：
%% 用 **dgiot 真实的调用姿势**（emqx:publish / emqx_broker:subscribe /
%% emqx_hooks / emqx_metrics / emqx_topic）驱动我们的内核，验证语义。
%%
%% 运行环境：**不加载 EMQX** 的干净节点（同名模块才有意义）。
%% 订阅者用普通进程，收到的投递是 gen_server:cast 形态的
%% {'$gen_cast', {deliver, Packet}} —— 正好也是 dgiot 侧 channel 进程
%% 真实会经历的路径。
-module(dgiot_broker_shadow_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok = dgiot_broker_router:init(),
    ok = dgiot_broker_router:reset(),
    ok = dgiot_broker_session:init(),
    ok = dgiot_broker_session:reset(),
    ok = emqx_hooks:init(),
    ok = emqx_hooks:reset(),
    ok = emqx_metrics:reset(),
    ok.

%% ---------------- VM 内发布/订阅（dgiot channel 的姿势） ----------------

inproc_publish_subscribe_test() ->
    setup(),
    Subscriber = spawn(fun loop/0),
    ?assertEqual(ok, emqx_broker:subscribe(Subscriber, <<"dgiot/#">>, 1)),
    Msg = emqx_message:make(<<"dgiot/siteA/gw1/dev1/pt1/data">>,
                            #{<<"v">> => 21.5}),
    {ok, Delivered} = emqx:publish(Msg),
    ?assertEqual(1, Delivered),
    ?assertEqual(1, emqx_metrics:val('messages.publish')),
    ?assertEqual(1, emqx_metrics:val('messages.delivered')),
    %% 订阅者确实收到了（拿回执，不看日志）
    Subscriber ! {report, self()},
    receive
        {got, Topic, Payload} ->
            ?assertEqual(<<"dgiot/siteA/gw1/dev1/pt1/data">>, Topic),
            ?assertEqual(<<"{\"v\":21.5}">>, Payload)
    after 1000 ->
            ?assert(false)
    end,
    Subscriber ! stop,
    setup().

inproc_publish_no_subscriber_test() ->
    setup(),
    Msg = emqx_message:make(<<"nobody/listening">>, <<"x">>),
    ?assertEqual({ok, 0}, emqx:publish(Msg)),
    ?assertEqual(1, emqx_metrics:val('messages.publish')),
    ?assertEqual(0, emqx_metrics:val('messages.delivered')),
    setup().

%% ---------------- 钩子面（dgiot 挂的 message.publish） ----------------

hooks_run_on_publish_test() ->
    setup(),
    Self = self(),
    ok = emqx:hook('message.publish', fun(Args) -> Self ! {hooked, Args} end, 1),
    ?assertEqual(1, length(emqx_hooks:lookup('message.publish'))),
    _ = emqx:publish(emqx_message:make(<<"t/1">>, <<"x">>)),
    receive
        {hooked, [Msg]} -> ?assertEqual(<<"t/1">>, emqx_message:topic(Msg))
    after 1000 ->
            ?assert(false)
    end,
    %% 卸载后不再触发
    ok = emqx:unhook('message.publish',
                     fun(_) -> Self ! {hooked, second} end),
    ?assertEqual(1, length(emqx_hooks:lookup('message.publish'))),
    ok = emqx_hooks:del('message.publish',
                        hd(emqx_hooks:lookup('message.publish'))),
    ?assertEqual(0, length(emqx_hooks:lookup('message.publish'))),
    setup().

crashing_hook_is_logged_not_swallowed_test() ->
    setup(),
    ok = emqx_hooks:add('message.publish', {erlang, error, [boom]}, 1),
    %% 回调崩了：publish 仍返回结果，但失败被计数（不静默）
    ?assertMatch({ok, _}, emqx:publish(emqx_message:make(<<"t/2">>, <<"y">>))),
    ?assertEqual(1, emqx_metrics:val('hooks.failed')),
    setup().

%% ---------------- 路由语义（含 EMQX 的静默点修正） ----------------

router_semantics_test() ->
    setup(),
    Self = self(),
    ok = emqx_broker:subscribe(Self, <<"dgiot/siteA/+/dev1/pt1/data">>, 0),
    %% 精确主题：有路由
    ?assert(emqx_router:has_routes(<<"dgiot/siteA/gw1/dev1/pt1/data">>)),
    %% 通配订阅也算有路由（EMQX 此处返回 false —— 静默丢弃的源头之一）
    ?assert(emqx_router:has_routes(<<"dgiot/siteA/gwX/dev1/pt1/data">>)),
    ?assertNot(emqx_router:has_routes(<<"other/topic">>)),
    ?assertEqual([Self], emqx_router:match_routes(
                           <<"dgiot/siteA/gw1/dev1/pt1/data">>)),
    ?assertEqual([], emqx_router:match_routes(<<"nomatch/topic">>)),
    setup().

topic_tools_test() ->
    ?assert(emqx_topic:match(<<"a/+/c">>, <<"a/b/c">>)),
    ?assertNot(emqx_topic:match(<<"a/+/c">>, <<"a/b/d">>)),
    ?assert(emqx_topic:validate(<<"a/#">>)),
    ?assertNot(emqx_topic:validate(<<"a/#/c">>)),
    ?assert(emqx_topic:wildcard(<<"a/+/c">>)),
    ?assertNot(emqx_topic:wildcard(<<"a/b/c">>)),
    ?assertEqual(<<"a/b/c">>, emqx_topic:join([<<"a">>, <<"b">>, <<"c">>])),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], emqx_topic:words(<<"a/b/c">>)).

%% ---------------- 工具面 ----------------

guid_test() ->
    G1 = emqx_guid:gen(),
    G2 = emqx_guid:gen(),
    ?assert(emqx_guid:is_guid(G1)),
    ?assertNotEqual(G1, G2),
    Hex = emqx_guid:gen_hex(),
    ?assertEqual(32, byte_size(Hex)),
    ?assertEqual(G1, emqx_guid:from_hex(emqx_guid:to_hex(G1))).

json_test() ->
    Term = #{<<"a">> => 1, <<"b">> => [1, 2]},
    Bin = emqx_json:encode(Term),
    ?assertEqual(Term, emqx_json:decode(Bin)),
    %% 不支持的选项显式报错，不静默忽略
    ?assertMatch({error, {unsupported_options, _}},
                 emqx_json:encode(Term, [pretty])).

metrics_test() ->
    emqx_metrics:reset(),
    ?assertEqual(0, emqx_metrics:val('unknown.metric')),
    emqx_metrics:ensure('a.b'),
    emqx_metrics:inc('a.b'),
    emqx_metrics:inc('a.b', 4),
    emqx_metrics:dec('a.b', 2),
    ?assertEqual(3, emqx_metrics:val('a.b')),
    ?assert(is_map(emqx_metrics:all())).

%% ---------------- 未实现的必须响亮 ----------------

unimplemented_is_loud_test() ->
    ?assertMatch({error, {not_implemented, _, emqx, reboot}}, emqx:reboot()),
    ?assertMatch({error, {not_implemented, _, server_side_subscribe}},
                 dgiot_broker_native:subscribe(<<"c">>, <<"t">>, #{})).

%% 测试用订阅进程：把投递回执给调用者
loop() ->
    receive
        {'$gen_cast', {deliver, Packet}} ->
            Topic = maps:get(topic, Packet, undefined),
            Payload = maps:get(payload, Packet, <<>>),
            put(last, {Topic, Payload}),
            loop();
        {report, From} ->
            From ! case get(last) of
                       undefined -> {got, undefined, undefined};
                       {T, P} -> {got, T, P}
                   end,
            loop();
        stop ->
            ok
    end.
