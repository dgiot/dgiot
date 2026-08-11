%%--------------------------------------------------------------------
%% dgiot_statem_demo — 状态机演示
%%
%% 启动后展示设备状态转移: offline → online → running → fault → recovery
%%--------------------------------------------------------------------
-module(dgiot_statem_demo).

-export([run/0, run/1]).

run() ->
    run(<<"dev-demo-001">>).

run(DeviceId) ->
    io:format("~n========== DGAIOT 状态机演示 ==========~n"),

    %% 1. 初始化 ETS
    dgiot_statem_model:init(),

    %% 2. 加载模型定义
    Model = model(),
    dgiot_statem_model:load(Model),
    io:format("[INIT] 模型已加载: ~s~n", [maps:get(<<"modelId">>, Model)]),

    %% 3. 启动状态机进程
    {ok, Sup} = dgiot_statem_sup:start_link(),
    {ok, Pid} = dgiot_statem_sup:start_child(DeviceId,
        maps:get(<<"modelId">>, Model), #{<<"health">> => 100}),
    io:format("[INIT] 设备 ~s 状态机已创建 (PID ~p)~n", [DeviceId, Pid]),

    %% 4. 模拟事件序列
    timer:sleep(500),
    events(DeviceId, Pid, [
        {<<"device_online">>,  #{}},
        {<<"data_received">>,  #{<<"health">> => 95}},
        {<<"health_low">>,     #{<<"health">> => 25}},
        {<<"fault_detected">>, #{}},
        {<<"fault_cleared">>,  #{}},
        {<<"data_received">>,  #{<<"health">> => 90}}
    ]),

    %% 5. 查看最终状态
    timer:sleep(500),
    #{
        state   := FinalState,
        history := History
    } = dgiot_statem:status(Pid),

    io:format("~n[RESULT] 最终状态: ~s~n", [FinalState]),
    io:format("[RESULT] 状态转移记录 (~p 次):~n", [length(History)]),
    lists:foreach(fun({From, To, Evt, Ts}) ->
        io:format("  ~s --(~s)--> ~s (at ~p)~n", [From, Evt, To, Ts])
    end, lists:reverse(History)),
    io:format("~n========== 演示结束 ==========~n"),
    {ok, FinalState, History}.

events(_DeviceId, _Pid, []) -> ok;
events(DeviceId, Pid, [{Event, Data} | Rest]) ->
    io:format("[EVENT] ~s ← ~s~n", [DeviceId, Event]),
    dgiot_statem:cast(Pid, {event, Event, Data}),
    timer:sleep(300),
    events(DeviceId, Pid, Rest).

%%--------------------------------------------------------------------
%% 示例模型: 逆变器
%%--------------------------------------------------------------------
model() ->
    #{
        <<"modelId">> => <<"solar_inverter">>,
        <<"class">> => <<"光伏逆变器">>,
        <<"initial">> => <<"offline">>,
        <<"states">> => #{
            <<"offline">> => #{
                <<"on">> => #{<<"device_online">> => <<"online">>}
            },
            <<"online">> => #{
                <<"entry">> => <<"log_online">>,
                <<"on">> => #{
                    <<"data_received">> => <<"running">>,
                    <<"device_offline">> => <<"offline">>
                }
            },
            <<"running">> => #{
                <<"entry">> => <<"start_monitor">>,
                <<"on">> => #{
                    <<"health_low">> => <<"warning">>,
                    <<"fault_detected">> => <<"fault">>,
                    <<"device_offline">> => <<"offline">>
                }
            },
            <<"warning">> => #{
                <<"on">> => #{
                    <<"data_received">> => <<"running">>,
                    <<"fault_detected">> => <<"fault">>
                },
                <<"timeout">> => 30
            },
            <<"fault">> => #{
                <<"entry">> => <<"raise_alarm">>,
                <<"on">> => #{
                    <<"fault_cleared">> => <<"running">>,
                    <<"device_offline">> => <<"offline">>
                }
            }
        },
        <<"actions">> => #{
            <<"log_online">> => #{<<"type">> => <<"log">>, <<"message">> => <<"设备上线">>},
            <<"start_monitor">> => #{<<"type">> => <<"log">>, <<"message">> => <<"开始监控">>},
            <<"raise_alarm">> => #{<<"type">> => <<"log">>, <<"message">> => <<"告警触发">>}
        }
    }.
