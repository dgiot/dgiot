%% dgiot_ontology_shadow — 第三层: 设备影子 gen_statem
%% 每个物理设备 = 一个 Erlang 进程 = 一个 gen_statem 实例
%% 实现: 属性更新(第一层) + 规则触发(第二层) + 实时执行(第三层)
-module(dgiot_ontology_shadow).
-behaviour(gen_statem).
-export([start_link/2, stop/1, update/2, get_state/1, get_properties/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([normal/3, warning/3, critical/3, offline/3]).

-define(HEARTBEAT_MS, 30000).  %% 30秒心跳超时
-define(WARNING_TIMEOUT, 60000). %% 告警状态60秒无变化→降级

-record(shadow, {
    device_id   :: binary(),
    class       :: binary(),
    properties  :: map(),       %% 当前属性 (第一层)
    model       :: map(),       %% 物模型定义
    rules       :: list(),      %% 编译后的规则 (第二层)
    last_update :: integer(),   %% UNIX时间戳
    heartbeat   :: reference()  %% 定时器引用
}).

%% ========== 公共 API ==========

start_link(Id, Model) ->
    gen_statem:start_link(?MODULE, {Id, Model}, []).

stop(Pid) ->
    gen_statem:stop(Pid).

update(Pid, Props) ->
    gen_statem:cast(Pid, {sensor_update, Props}).

get_state(Pid) ->
    gen_statem:call(Pid, get_state).

get_properties(Pid) ->
    gen_statem:call(Pid, get_properties).

%% ========== gen_statem 回调 ==========

callback_mode() -> [state_functions, state_enter].

init({Id, Model}) ->
    Rules = dgiot_ontology_rule:compile(maps:get(rules, Model, [])),
    Shadow = #shadow{
        device_id   = maps:get(<<"id">>, Model, Id),
        class       = maps:get(<<"class">>, Model, <<"unknown">>),
        properties  = #{},
        model       = Model,
        rules       = Rules,
        last_update = os:system_time(seconds)
    },
    {ok, normal, Shadow, [{next_event, internal, log_init}]}.

%% ========== 状态: normal ==========

normal(enter, _Old, Data) ->
    %% 进入normal状态: 重置定时器, 发送心跳
    Heartbeat = erlang:start_timer(?HEARTBEAT_MS, self(), heartbeat),
    {keep_state, Data#shadow{heartbeat = Heartbeat}};

normal(cast, {sensor_update, Props}, #shadow{rules = Rules, properties = Old} = Data) ->
    NewProps = maps:merge(Old, Props),
    NewData = Data#shadow{properties = NewProps, last_update = os:system_time(seconds)},

    %% ① 更新影子属性 (第一层: 知识图谱)
    dgiot_ontology_registry:update_properties(Data#shadow.device_id, NewProps),

    %% ② 评估规则 (第二层: SWRL推理)
    case evaluate_rules(Rules, NewProps) of
        [{critical, Action} | _] ->
            %% ③ L1全自动: ESD关断·<2s (第三层: 实时执行)
            execute_action(Action, Data),
            {next_state, critical, NewData};
        [{warn, Action} | _] ->
            %% L2人机协同: 推送告警 + 30s超时默认保护
            notify_operator(warn, Action, Data),
            Actions = [{state_timeout, ?WARNING_TIMEOUT, auto_protect}],
            {next_state, warning, NewData, Actions};
        [] ->
            {keep_state, NewData};
        {ok, none} ->
            {keep_state, NewData}
    end;

normal(info, {heartbeat_tick, _Ref}, Data) ->
    %% 心跳正常——当前无动作
    {keep_state, Data};

normal(cast, check_heartbeat, #shadow{last_update = Last} = Data) ->
    Now = os:system_time(seconds),
    case Now - Last of
        T when T > 30 ->
            %% 信任衰减律 L6: 30秒无更新→标记offline→切换冗余
            dgiot_ontology_registry:mark_offline(Data#shadow.device_id),
            {next_state, offline, Data};
        _ -> {keep_state, Data}
    end;

normal({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, normal}]};
normal({call, From}, get_properties, #shadow{properties = P}) ->
    {keep_state_and_data, [{reply, From, P}]}.

%% ========== 状态: warning ==========

warning(enter, _Old, Data) ->
    io:format("[SHADOW] ~s → WARNING~n", [Data#shadow.device_id]),
    {keep_state, Data};

warning(cast, {sensor_update, Props}, Data) ->
    %% 告警状态下仍继续更新+评估
    NewProps = maps:merge(Data#shadow.properties, Props),
    NewData = Data#shadow{properties = NewProps, last_update = os:system_time(seconds)},

    case evaluate_rules(Data#shadow.rules, NewProps) of
        [{critical, Action} | _] ->
            execute_action(Action, Data),
            {next_state, critical, NewData};
        [] ->
            %% 值恢复正常→回normal
            {next_state, normal, NewData};
        _ ->
            {keep_state, NewData}
    end;

warning(state_timeout, auto_protect, Data) ->
    %% L2超时默认保护: 30s无人确认→执行默认动作
    io:format("[SHADOW] ~s L2 timeout → auto-protect~n", [Data#shadow.device_id]),
    execute_action({esd, default_protect}, Data),
    {next_state, critical, Data}.

%% ========== 状态: critical ==========

critical(enter, _Old, Data) ->
    io:format("[SHADOW] ~s → CRITICAL · ESD可能已触发~n", [Data#shadow.device_id]),
    {keep_state, Data};

critical(cast, {sensor_update, Props}, Data) ->
    NewProps = maps:merge(Data#shadow.properties, Props),
    NewData = Data#shadow{properties = NewProps},
    %% critical状态下继续监测, 但不再触发新ESD
    case evaluate_rules(Data#shadow.rules, NewProps) of
        [] ->
            {next_state, normal, NewData};
        _ ->
            {keep_state, NewData}
    end;

critical(info, {resolved, Reason}, Data) ->
    %% 闭环验证 L8: 维修后重新评估
    io:format("[SHADOW] ~s resolved: ~s → normal~n", [Data#shadow.device_id, Reason]),
    dgiot_ontology_registry:log_event(Data#shadow.device_id, resolved, Reason),
    {next_state, normal, Data}.

%% ========== 状态: offline ==========

offline(enter, _Old, Data) ->
    io:format("[SHADOW] ~s → OFFLINE~n", [Data#shadow.device_id]),
    {keep_state, Data};

offline(cast, {sensor_update, Props}, Data) ->
    %% 心跳恢复→回normal
    NewData = Data#shadow{properties = maps:merge(Data#shadow.properties, Props),
                          last_update = os:system_time(seconds)},
    dgiot_ontology_registry:mark_online(Data#shadow.device_id),
    {next_state, normal, NewData}.

%% ========== 内部函数 ==========

%% 规则评估器: 匹配 gen_statem 守卫 (第二层)
evaluate_rules([], _Props) -> [];
evaluate_rules([{Guard, Action, Level} | Rest], Props) ->
    case eval_guard(Guard, Props) of
        true -> [{Level, Action} | evaluate_rules(Rest, Props)];
        false -> evaluate_rules(Rest, Props)
    end.

eval_guard(#{<<"vibration">> := V, <<"temperature">> := T}, Props)
  when is_map_key(<<"vibration">>, Props), is_map_key(<<"temperature">>, Props) ->
    PV = maps:get(<<"vibration">>, Props),
    PT = maps:get(<<"temperature">>, Props),
    PV > V andalso PT > T;
eval_guard(#{<<"vibration">> := V}, Props) when is_map_key(<<"vibration">>, Props) ->
    maps:get(<<"vibration">>, Props) > V;
eval_guard(#{<<"pressure">> := P}, Props) when is_map_key(<<"pressure">>, Props) ->
    maps:get(<<"pressure">>, Props) > P;
eval_guard(#{<<"flow">> := F}, Props) when is_map_key(<<"flow">>, Props) ->
    maps:get(<<"flow">>, Props) < F;
eval_guard(_, _) -> false.

%% 动作执行器 (第三层)
execute_action({esd, Reason}, #shadow{device_id = Id}) ->
    io:format("[ESD] Device ~s: ~s~n", [Id, Reason]),
    dgiot_ontology_registry:log_event(Id, esd, Reason),
    spawn(fun() -> opcua_command:emergency_shutdown(Id, Reason) end);

execute_action({alarm, Level, Msg}, #shadow{device_id = Id}) ->
    dgiot_ontology_registry:raise_alarm(Id, Level, Msg);

execute_action({set_sampling, Hz}, #shadow{device_id = Id}) ->
    spawn(fun() -> opcua_command:set_sampling_rate(Id, Hz) end);

execute_action(_, _) -> ok.

notify_operator(Level, Action, #shadow{device_id = Id}) ->
    spawn(fun() ->
        Msg = io_lib:format("[~s] ~s: ~p", [Level, Id, Action]),
        dgiot_ontology_registry:push_notification(Id, Msg)
    end).

%% ========== 生命周期 ==========

terminate(_Reason, _State, #shadow{heartbeat = Hb}) ->
    erlang:cancel_timer(Hb),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
