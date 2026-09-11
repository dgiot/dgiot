%% @doc 刀 3 单测：路由匹配、会话登记与接管、认证钩子。
-module(dgiot_broker_core_tests).
-include_lib("eunit/include/eunit.hrl").

%% 认证回调测试需要本模块作为 callback module，故显式导出
-export([authenticate/3]).

%% ---------------- 路由：过滤器合法性 ----------------

filter_validation_test() ->
    ?assertEqual(ok, dgiot_broker_router:validate_filter(<<"dgiot/#">>)),
    ?assertEqual(ok, dgiot_broker_router:validate_filter(<<"a/+/c">>)),
    ?assertEqual(ok, dgiot_broker_router:validate_filter(<<"#">>)),
    ?assertEqual(ok, dgiot_broker_router:validate_filter(<<"+">>)),
    ?assertEqual(ok, dgiot_broker_router:validate_filter(<<"a/b/c">>)),
    ?assertEqual({error, empty_filter},
                 dgiot_broker_router:validate_filter(<<>>)),
    ?assertEqual({error, hash_not_last},
                 dgiot_broker_router:validate_filter(<<"a/#/c">>)),
    ?assertMatch({error, {hash_in_level, _}},
                 dgiot_broker_router:validate_filter(<<"a#/c">>)),
    ?assertMatch({error, {plus_in_level, _}},
                 dgiot_broker_router:validate_filter(<<"a+/c">>)).

%% ---------------- 路由：主题匹配 ----------------

topic_match_test() ->
    ?assert(dgiot_broker_router:topic_matches(
              <<"dgiot/siteA/gw1/dev1/pt1/data">>,
              <<"dgiot/siteA/gw1/dev1/pt1/data">>)),
    ?assertNot(dgiot_broker_router:topic_matches(
                 <<"dgiot/siteA/gw1/dev1/pt1/data">>,
                 <<"dgiot/siteA/gw1/dev1/pt1/state">>)),
    %% 单层通配
    ?assert(dgiot_broker_router:topic_matches(
              <<"dgiot/+/gw1/dev1/pt1/data">>, <<"dgiot/siteA/gw1/dev1/pt1/data">>)),
    ?assertNot(dgiot_broker_router:topic_matches(
                 <<"dgiot/+/gw1/dev1/pt1/data">>,
                 <<"dgiot/siteA/gw2/dev1/pt1/data">>)),
    %% 多层通配
    ?assert(dgiot_broker_router:topic_matches(
              <<"dgiot/#">>, <<"dgiot/anything/deep/here">>)),
    ?assert(dgiot_broker_router:topic_matches(<<"#">>, <<"any/topic/at/all">>)),
    %% 层级数不同不匹配
    ?assertNot(dgiot_broker_router:topic_matches(<<"a/b">>, <<"a/b/c">>)),
    ?assertNot(dgiot_broker_router:topic_matches(<<"a/b/c">>, <<"a/b">>)).

dollar_topic_protection_test() ->
    %% 通配订阅不得命中 $ 开头主题（MQTT 3.1.1 §4.7.2）
    ?assertNot(dgiot_broker_router:topic_matches(
                 <<"#">>, <<"$SYS/broker/uptime">>)),
    ?assertNot(dgiot_broker_router:topic_matches(
                 <<"+/broker/uptime">>, <<"$SYS/broker/uptime">>)),
    %% 显式订阅 $ 主题是允许的
    ?assert(dgiot_broker_router:topic_matches(
              <<"$SYS/#">>, <<"$SYS/broker/uptime">>)),
    ?assert(dgiot_broker_router:topic_matches(
              <<"$dg/device/+/+/debug">>,
              <<"$dg/device/p1/dev1/debug">>)).

%% ---------------- 路由：注册与匹配 ----------------

router_registry_test() ->
    dgiot_broker_router:init(),
    dgiot_broker_router:reset(),
    Pid = self(),
    ok = dgiot_broker_router:subscribe(<<"dgiot/#">>, <<"c1">>, Pid, 1),
    ok = dgiot_broker_router:subscribe(<<"dgiot/siteA/+/dev1/pt1/data">>,
                                       <<"c2">>, Pid, 0),
    ?assertEqual(2, dgiot_broker_router:count()),
    M1 = dgiot_broker_router:match(<<"dgiot/siteA/gw1/dev1/pt1/data">>),
    ?assertEqual(2, length(M1)),
    M2 = dgiot_broker_router:match(<<"dgiot/siteA/gw1/dev2/pt1/data">>),
    ?assertEqual(1, length(M2)),
    %% 非法过滤器必须显式报错，不得入库
    ?assertMatch({error, _},
                 dgiot_broker_router:subscribe(<<"bad/#/x">>, <<"c3">>, Pid, 1)),
    ?assertMatch({error, {bad_qos, 5}},
                 dgiot_broker_router:subscribe(<<"ok/topic">>, <<"c3">>, Pid, 5)),
    ?assertEqual(2, dgiot_broker_router:count()),
    ?assertEqual(1, dgiot_broker_router:unsubscribe_all(<<"c1">>)),
    ?assertEqual(1, dgiot_broker_router:unsubscribe_all(<<"c2">>)),
    ?assertEqual(0, dgiot_broker_router:count()),
    dgiot_broker_router:reset().

%% ---------------- 会话 ----------------

session_lifecycle_test() ->
    dgiot_broker_session:init(),
    dgiot_broker_session:reset(),
    {ok, S} = dgiot_broker_session:register(<<"c1">>, self(),
                                            #{username => <<"admin">>,
                                              keepalive => 30}),
    ?assertEqual(<<"admin">>, maps:get(username, S)),
    ?assertEqual(1, dgiot_broker_session:count()),
    {ok, S2} = dgiot_broker_session:lookup(<<"c1">>),
    ?assertEqual(30, maps:get(keepalive, S2)),
    %% 局部更新保留原字段
    ok = dgiot_broker_session:update(<<"c1">>,
                                     fun(M) -> M#{subscriptions => [<<"a/#">>]} end),
    {ok, S3} = dgiot_broker_session:lookup(<<"c1">>),
    ?assertEqual([<<"a/#">>], maps:get(subscriptions, S3)),
    ?assertEqual(<<"admin">>, maps:get(username, S3)),
    dgiot_broker_session:unregister(<<"c1">>),
    ?assertEqual({error, not_found}, dgiot_broker_session:lookup(<<"c1">>)),
    dgiot_broker_session:reset().

session_takeover_test() ->
    dgiot_broker_session:init(),
    dgiot_broker_session:reset(),
    {ok, _} = dgiot_broker_session:register(<<"dup">>, self(), #{}),
    FakeOld = spawn(fun() -> timer:sleep(1000) end),
    %% 同一 ClientId、不同 pid → 显式报告接管（不静默顶掉）
    ?assertEqual({takeover, self()},
                 dgiot_broker_session:register(<<"dup">>, FakeOld, #{})),
    exit(FakeOld, kill),
    dgiot_broker_session:reset().

%% ---------------- 认证钩子 ----------------

auth_default_allow_test() ->
    application:set_env(dgiot_broker, auth, #{mode => allow_all}),
    ?assertEqual(ok, dgiot_broker_auth:authenticate(<<"c1">>, <<"u">>, <<"p">>)),
    %% 空 ClientId 必须拒绝
    ?assertMatch({error, empty_client_id},
                 dgiot_broker_auth:authenticate(<<>>, <<"u">>, <<"p">>)),
    ?assertMatch({error, no_client_id},
                 dgiot_broker_auth:authenticate(undefined, <<"u">>, <<"p">>)).

auth_callback_test() ->
    application:set_env(dgiot_broker, auth,
                        #{mode => callback, module => dgiot_broker_core_tests}),
    ?assertEqual(ok, dgiot_broker_auth:authenticate(<<"ok-client">>, <<"u">>, <<"p">>)),
    ?assertMatch({error, bad_credentials},
                 dgiot_broker_auth:authenticate(<<"bad-client">>, <<"u">>, <<"p">>)),
    %% 回调返回意外值 → 拒绝（不得当放行）
    ?assertMatch({error, {bad_callback_return, _}},
                 dgiot_broker_auth:authenticate(<<"weird-client">>, <<"u">>, <<"p">>)),
    application:set_env(dgiot_broker, auth, #{mode => allow_all}).

%% 供认证回调测试使用的假回调（本模块被当作 callback module）
authenticate(<<"ok-client">>, _U, _P) -> ok;
authenticate(<<"bad-client">>, _U, _P) -> {error, bad_credentials};
authenticate(_Other, _U, _P) -> unexpected_value.
