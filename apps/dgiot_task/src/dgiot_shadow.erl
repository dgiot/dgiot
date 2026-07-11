%%--------------------------------------------------------------------
%% dgiot_shadow — 影子设备 & 状态机
%%
%% 每个物理设备对应一个 gen_statem 进程
%% 状态: init → auth → online → {normal, alarm, offline}
%% MQTT 消息驱动状态变迁，影子状态同步到 Parse
%%--------------------------------------------------------------------
-module(dgiot_shadow).
-behaviour(gen_statem).
-author("edge-hub").

%% API
-export([start_link/1, start_link/2,
         get_state/1, get_device/1,
         inject/2,          %% MQTT 消息注入
         sync_to_parse/1    %% 同步到 Parse
        ]).

%% gen_statem
-export([init/1, callback_mode/0,
         authenticate/3, online/3, alarm/3, offline/3,
         terminate/3, code_change/4]).

-include_lib("dgiot/include/logger.hrl").

%% ——— 记录 ———
-record(device, {
    id,                    %% 设备ID
    product_id,            %% 产品ID
    name,                  %% 名称
    type,                  %% rtu/sensor/gateway
    ontology_path,         %% 本体路径: dgiot/{site}/{gateway}/{device}
    metadata = #{},        %% 设备元数据
    points = [],           %% 测点列表
    status = init,         %% 当前状态
    last_online,           %% 最后在线时间
    error_count = 0        %% 连续错误计数
}).

%% ——— 状态定义 ———
-define(MAX_ERRORS, 3).        %% 连续 N 次错误 → off
-define(HEARTBEAT_TIMEOUT, 120000).  %% 120s 无心跳 → offline

%% ——— API ———
start_link(DeviceId) ->
    start_link(DeviceId, #{}).

start_link(DeviceId, Opts) ->
    gen_statem:start_link(?MODULE, [DeviceId, Opts], []).

get_state(Pid) ->
    gen_statem:call(Pid, get_state).

get_device(Pid) ->
    gen_statem:call(Pid, get_device).

%% MQTT 消息注入
inject(Pid, {data, Value}) ->
    gen_statem:cast(Pid, {data, Value});
inject(Pid, {event, Event}) ->
    gen_statem:cast(Pid, {event, Event});
inject(Pid, heartbeat) ->
    gen_statem:cast(Pid, heartbeat).

sync_to_parse(Pid) ->
    gen_statem:call(Pid, sync).

%% ——— gen_statem ———
callback_mode() -> state_functions.

init([DeviceId, Opts]) ->
    %% 从本体引擎加载设备定义
    {ok, Device} = load_device(DeviceId, Opts),
    {ok, authenticate, Device, [{next_event, internal, enter}]}.

%% ============ authenticate ============
authenticate(internal, enter, Device) ->
    io_lib:format("[~s] auth -> online", [Device#device.id]),
    %% TODO: TLS/Token 验证
    {next_state, online, Device#device{status = online}, [{state_timeout, ?HEARTBEAT_TIMEOUT, heartbeat_missed}]};

authenticate({call, From}, _, _Device) ->
    {keep_state_and_data, [{reply, From, {error, authenticating}}]};

authenticate(cast, _, Device) ->
    {keep_state, Device}.

%% ============ online (正常) ============
online(internal, enter, Device) ->
    sync_parse(Device),
    {keep_state, Device};

online(state_timeout, heartbeat_missed, Device) ->
    logger:warning("[~s] heartbeat missed, -> offline", [Device#device.id]),
    {next_state, offline, Device#device{status = offline}};

online(cast, {data, Values}, Device) ->
    NewDevice = update_points(Device, Values),
    sync_parse(NewDevice),
    {keep_state, NewDevice#device{error_count = 0}, [{state_timeout, ?HEARTBEAT_TIMEOUT, heartbeat_missed}]};

online(cast, heartbeat, Device) ->
    NewDevice = Device#device{last_online = erlang:system_time(second), error_count = 0},
    {keep_state, NewDevice, [{state_timeout, ?HEARTBEAT_TIMEOUT, heartbeat_missed}]};

online(cast, {event, #{<<"level">> := <<"error">>} = Event}, Device) ->
    logger:error("[~s] alarm: ~p", [Device#device.id, Event]),
    NewDevice = Device#device{error_count = Device#device.error_count + 1},
    case NewDevice#device.error_count >= ?MAX_ERRORS of
        true ->
            {next_state, alarm, NewDevice#device{status = alarm}};
        false ->
            {keep_state, NewDevice, [{state_timeout, ?HEARTBEAT_TIMEOUT, heartbeat_missed}]}
    end;

online({call, From}, get_state, Device) ->
    {keep_state, Device, [{reply, From, Device#device.status}]};

online({call, From}, get_device, Device) ->
    {keep_state, Device, [{reply, From, Device}]};

online({call, From}, sync, Device) ->
    sync_parse(Device),
    {keep_state, Device, [{reply, From, ok}]}.

%% ============ alarm (告警) ============
alarm(internal, enter, Device) ->
    sync_parse(Device),
    {keep_state, Device, [{state_timeout, ?HEARTBEAT_TIMEOUT, heartbeat_missed}]};

alarm(state_timeout, heartbeat_missed, Device) ->
    {next_state, offline, Device#device{status = offline}};

alarm(cast, heartbeat, Device) ->
    {next_state, online, Device#device{status = online, error_count = 0}};

alarm(cast, {data, _}, Device) ->
    %% 告警态仍收数据但不降级
    {keep_state, Device, [{state_timeout, ?HEARTBEAT_TIMEOUT, heartbeat_missed}]};

alarm({call, From}, get_state, Device) ->
    {keep_state, Device, [{reply, From, Device#device.status}]};

alarm({call, From}, get_device, Device) ->
    {keep_state, Device, [{reply, From, Device}]}.

%% ============ offline ============
offline(internal, enter, Device) ->
    logger:warning("[~s] offline", [Device#device.id]),
    sync_parse(Device),
    {keep_state, Device};

offline(cast, heartbeat, Device) ->
    {next_state, online, Device#device{status = online, error_count = 0}};

offline({call, From}, get_state, Device) ->
    {keep_state, Device, [{reply, From, Device#device.status}]}.

%% ——— 内部函数 ———
load_device(DeviceId, Opts) ->
    %% 从 Parse 或本体引擎加载设备
    {ok, #{<<"name">> := Name, <<"type">> := Type} = Dev} =
        dgiot_parse:get_object(<<"Device">>, DeviceId),
    OntPath = dgiot_ontology:get_path(DeviceId),
    {ok, #device{
        id = DeviceId,
        name = binary_to_list(Name),
        type = binary_to_list(Type),
        ontology_path = OntPath,
        metadata = Opts,
        points = dgiot_ontology:get_points(DeviceId),
        status = init,
        last_online = erlang:system_time(second)
    }}.

update_points(Device, Values) ->
    %% 更新测点值, 推送到 MQTT
    lists:foreach(fun({PointId, V}) ->
        dgiot_ontology:push_point(PointId, V)
    end, maps:to_list(Values)),
    Device#device{last_online = erlang:system_time(second)}.

sync_parse(Device) ->
    %% 同步影子状态到 Parse
    dgiot_parse:update_object(<<"Device">>, Device#device.id, #{
        <<"status">> => Device#device.status,
        <<"last_online">> => Device#device.last_online,
        <<"ontology_path">> => Device#device.ontology_path
    }).

terminate(_Reason, _State, _Device) -> ok.
code_change(_OldVsn, State, Device, _Extra) -> {ok, State, Device}.
