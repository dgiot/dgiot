%%% dgiot_ontology_hook — 设备数据流钩子 (插入dgiot事件管道)
%%%
%%% 挂载点: dgiot_bridge 数据到达时 → 本体语义检查 → 规则触发 → 动作执行
%%% 零侵入: 不修改现有dgiot代码, 通过hook机制注册
-module(dgiot_ontology_hook).
-export([on_message/2, on_device_online/1, on_device_offline/1]).
-export([register_hooks/0, status/0]).

%% ═══ Hook注册 ═══
%% 在 dgiot 启动后调用此函数注册钩子
register_hooks() ->
    io:format("[ONTOLOGY-HOOK] Registering data pipeline hooks~n"),
    dgiot_ontology:start(),
    %% 加载默认本体模型
    load_default_models(),
    {ok, #{hooks => 3, status => active}}.

load_default_models() ->
    %% 通用设备健康模型
    dgiot_ontology:load_model(#{
        <<"modelId">> => <<"DeviceHealth-v1">>,
        <<"class">> => <<"equipment">>,
        <<"properties">> => [
            #{<<"id">> => <<"health_score">>, <<"type">> => <<"float">>},
            #{<<"id">> => <<"temperature">>, <<"type">> => <<"float">>},
            #{<<"id">> => <<"vibration">>, <<"type">> => <<"float">>}
        ],
        <<"relations">> => [
            #{<<"relation">> => <<"monitored_by">>, <<"target">> => <<"quality">>}
        ],
        <<"rules">> => [
            #{<<"id">> => <<"P2">>, <<"severity">> => <<"L1">>,
              <<"when">> => #{<<"property">> => <<"health_score">>, <<"op">> => <<"<">>, <<"value">> => 60},
              <<"then">> => #{<<"state">> => <<"fault">>, <<"action">> => <<"emergency_stop">>}},
            #{<<"id">> => <<"P1">>, <<"severity">> => <<"L3">>,
              <<"when">> => #{<<"property">> => <<"health_score">>, <<"op">> => <<"<">>, <<"value">> => 80},
              <<"then">> => #{<<"state">> => <<"warning">>, <<"action">> => <<"alert">>}},
            #{<<"id">> => <<"T1">>, <<"severity">> => <<"L2">>,
              <<"when">> => #{<<"property">> => <<"temperature">>, <<"op">> => <<">">>, <<"value">> => 85},
              <<"then">> => #{<<"state">> => <<"warning">>, <<"action">> => <<"adjust_cooling">>}}
        ]
    }),
    ok.

%% ═══ 数据管道钩子 ═══

%% MQTT消息到达 (从 dgiot_bridge 调用)
%% Message = #{device_id => binary(), properties => map(), timestamp => integer()}
on_message(DeviceId, #{<<"properties">> := Props} = _Msg) ->
    case dgiot_ontology_registry:lookup(DeviceId) of
        {ok, _Entry} ->
            %% 设备已注册 — 执行语义验证+规则检查
            case dgiot_ontology_bridge:evaluate(DeviceId, Props) of
                {triggered, Actions} ->
                    io:format("[ONTOLOGY-HOOK] ~s: TRIGGERED ~p actions~n",
                        [DeviceId, length(Actions)]),
                    %% 将触发的动作写入数据库
                    [dgiot_ontology_pg:audit_log(DeviceId, rule_triggered,
                        #{}, #{action => A}) || A <- Actions],
                    {ok, Actions};
                {ok, none} ->
                    {ok, []}
            end;
        {error, _} ->
            %% 新设备 — 自动注册
            dgiot_ontology_registry:register(equipment, DeviceId, Props),
            {ok, auto_registered}
    end;

on_message(DeviceId, Msg) ->
    io:format("[ONTOLOGY-HOOK] ~s: unknown format ~p~n", [DeviceId, Msg]),
    {ok, ignored}.

%% 设备上线
on_device_online(DeviceId) ->
    dgiot_ontology_registry:register(equipment, DeviceId,
        #{<<"state">> => <<"online">>, <<"last_seen">> => erlang:system_time()}),
    dgiot_ontology_pg:audit_log(DeviceId, online, #{}, #{}),
    ok.

%% 设备离线
on_device_offline(DeviceId) ->
    dgiot_ontology_pg:audit_log(DeviceId, offline, #{}, #{}),
    ok.

%% ═══ 状态查询 ═══
status() ->
    #{
        models => length(dgiot_ontology:list_models()),
        entities => dgiot_ontology_registry:count(),
        hooks => [on_message, on_device_online, on_device_offline]
    }.
