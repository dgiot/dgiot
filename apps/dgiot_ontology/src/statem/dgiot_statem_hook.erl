%%--------------------------------------------------------------------
%% dgiot_statem_hook — bridge between dgiot_hook and gen_statem engine
%%
%% Registers hooks at startup so device data flows into the state machine.
%%
%% Hook registration:
%%   dgiot_hook:add({tcp, ProductId}, {dgiot_statem_hook, on_message})
%%   dgiot_hook:add({httpc, ProductId}, {dgiot_statem_hook, on_message})
%%   dgiot_hook:add(<<"device/+/online">>, {dgiot_statem_hook, on_online})
%%--------------------------------------------------------------------
-module(dgiot_statem_hook).

-export([register/0, register/1]).
-export([on_message/2, on_online/1, on_offline/1]).

%%--------------------------------------------------------------------
%% Hook registration
%%--------------------------------------------------------------------

%% Register hooks for all known product IDs
register() ->
    dgiot_statem_model:init(),
    io:format("[STATEM-HOOK] Registering data pipeline hooks~n"),
    ok.

%% Register hooks for a specific product ID
register(ProductId) when is_binary(ProductId) ->
    %% Register on TCP channel data
    dgiot_hook:add({tcp, ProductId}, {?MODULE, on_message}),
    %% Register on HTTP channel data
    dgiot_hook:add({httpc, ProductId}, {?MODULE, on_message}),
    ok.

%%--------------------------------------------------------------------
%% Hook callbacks — called by dgiot_hook:run_hook
%%--------------------------------------------------------------------

%% Device data arrives via channel
on_message(DeviceId, Msg) when is_map(Msg) ->
    case find_statem(DeviceId) of
        {ok, Pid} ->
            dgiot_statem:cast(Pid, {event, data_received, Msg}),
            {ok, dispatched};
        {error, not_found} ->
            %% Auto-create state machine for new device
            case auto_start(DeviceId, Msg) of
                {ok, Pid} ->
                    dgiot_statem:cast(Pid, {event, device_online, Msg}),
                    {ok, auto_created};
                {error, _} = E -> E
            end
    end;
on_message(_DeviceId, _Msg) ->
    {error, invalid_message}.

%% Device comes online
on_online(DeviceId) when is_binary(DeviceId) ->
    case find_statem(DeviceId) of
        {ok, Pid} ->
            dgiot_statem:cast(Pid, {event, device_online, #{}}),
            {ok, online};
        {error, not_found} ->
            auto_start(DeviceId, #{})
    end.

%% Device goes offline
on_offline(DeviceId) when is_binary(DeviceId) ->
    case find_statem(DeviceId) of
        {ok, Pid} ->
            dgiot_statem:cast(Pid, {event, device_offline, #{}}),
            {ok, offline};
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

find_statem(DeviceId) ->
    case ets:lookup(dgiot_statem_pid, DeviceId) of
        [{DeviceId, Pid}] ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false ->
                    ets:delete(dgiot_statem_pid, DeviceId),
                    {error, not_found}
            end;
        [] -> {error, not_found}
    end.

auto_start(DeviceId, Msg) ->
    ModelId = guess_model(DeviceId, Msg),
    case dgiot_statem_sup:start_child(DeviceId, ModelId, Msg) of
        {ok, Pid} ->
            ensure_ets(),
            ets:insert(dgiot_statem_pid, {DeviceId, Pid}),
            {ok, Pid};
        {ok, Pid, _} ->
            ensure_ets(),
            ets:insert(dgiot_statem_pid, {DeviceId, Pid}),
            {ok, Pid};
        {error, _} = E -> E
    end.

ensure_ets() ->
    case ets:info(dgiot_statem_pid) of
        undefined -> ets:new(dgiot_statem_pid, [named_table, public, set]);
        _ -> ok
    end.

guess_model(_DeviceId, #{<<"product">> := Product}) -> Product;
guess_model(_DeviceId, #{<<"modelId">> := MId}) -> MId;
guess_model(_DeviceId, _) -> <<"default">>.
