%%--------------------------------------------------------------------
%% dgiot_statem — gen_statem per device (handle_event_function mode)
%%
%% Each device instance runs as an independent gen_statem process.
%% Model definition loaded from dgiot_statem_model (ETS).
%% State entry tracked manually since states are dynamic atoms.
%%--------------------------------------------------------------------
-module(dgiot_statem).

-behaviour(gen_statem).

-export([start_link/2, start_link/3]).
-export([cast/2, call/2, stop/1, status/1]).

%% gen_statem
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([handle_event/4]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link(DeviceId, ModelId) ->
    start_link(DeviceId, ModelId, #{}).

start_link(DeviceId, ModelId, Data) ->
    gen_statem:start_link(?MODULE, {DeviceId, ModelId, Data}, []).

cast(Pid, Event) -> gen_statem:cast(Pid, Event).
call(Pid, Event) -> gen_statem:call(Pid, Event).
stop(Pid) -> gen_statem:stop(Pid).
status(Pid) -> gen_statem:call(Pid, status).

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------

callback_mode() -> handle_event_function.

init({DeviceId, ModelId, Data}) ->
    process_flag(trap_exit, true),
    case dgiot_statem_model:get(ModelId) of
        {ok, Model} ->
            Initial = maps:get(initial, Model, idle),
            St = #{
                current => Initial,
                device  => DeviceId,
                model   => ModelId,
                model_m => Model,
                props   => Data,
                history => [],
                started => erlang:system_time(second)
            },
            %% Trigger entry action for initial state
            on_enter(St),
            log(DeviceId, init, ModelId, Initial),
            {ok, Initial, St};
        {error, _} ->
            {stop, {model_not_found, ModelId}}
    end.

%%--------------------------------------------------------------------
%% handle_event/4 — single handler for all events
%%--------------------------------------------------------------------

%% Status query
handle_event({call, From}, status, _State, St) ->
    {keep_state_and_data, [{reply, From, #{
        state   => maps:get(current, St),
        device  => maps:get(device, St),
        model   => maps:get(model, St),
        props   => maps:get(props, St),
        history => maps:get(history, St)
    }}]};

%% Device event → check for transition
handle_event(cast, {event, EventType, EventData}, _State, St) ->
    handle_event_cast(EventType, EventData, St);

%% Update device properties
handle_event(cast, {update, Props}, State, St) ->
    {keep_state, State, St#{props => Props}};

%% Info from parent
handle_event(info, {device_data, Props}, State, St) ->
    handle_info_data(Props, State, St);

handle_event(info, _Info, State, St) ->
    {keep_state, State, St};

%% Timeout
handle_event(state_timeout, _T, State, St) ->
    log(maps:get(device, St), timeout, State, State),
    keep_state_and_data;

%% Default
handle_event(EventType, EventContent, State, St) ->
    log(maps:get(device, St), EventType, EventContent, State),
    keep_state_and_data.

terminate(_Reason, _State, #{device := Dev}) ->
    log(Dev, stop, terminate, _Reason),
    ok.

code_change(_OldVsn, State, St, _Extra) ->
    {ok, State, St}.

%%--------------------------------------------------------------------
%% Transition logic
%%--------------------------------------------------------------------

handle_event_cast(EventType, EventData, St) ->
    State = maps:get(current, St),
    Model = maps:get(model_m, St),
    States = maps:get(states, Model, #{}),
    StateDef = maps:get(State, States, #{}),
    Events = maps:get(events, StateDef, #{}),

    Key = to_event_key(EventType),
    case maps:find(Key, Events) of
        {ok, NextState} ->
            Dev = maps:get(device, St),
            log(Dev, EventType, State, NextState),
            History = [{State, NextState, Key, erlang:system_time(second)}
                       | maps:get(history, St)],
            NewSt = St#{current := NextState, history => History},
            on_enter(NewSt),
            {next_state, NextState, NewSt};
        error ->
            keep_state_and_data
    end.

handle_info_data(#{<<"status">> := S} = M, State, St) ->
    handle_event_cast(S, M, St);
handle_info_data(#{<<"event">> := E} = M, State, St) ->
    handle_event_cast(E, M, St);
handle_info_data(#{<<"fault">> := true}, State, St) ->
    handle_event_cast(fault_detected, #{}, St);
handle_info_data(#{<<"fault">> := false}, State, St) ->
    handle_event_cast(fault_cleared, #{}, St);
handle_info_data(#{<<"health">> := H}, State, St) when H < 30 ->
    handle_event_cast(health_low, #{health => H}, St);
handle_info_data(_, State, St) ->
    {keep_state, State, St}.

%%--------------------------------------------------------------------
%% State entry
%%--------------------------------------------------------------------

on_enter(St) ->
    State = maps:get(current, St),
    Model = maps:get(model_m, St),
    States = maps:get(states, Model, #{}),
    StateDef = maps:get(State, States, #{}),
    case maps:get(entry, StateDef, undefined) of
        undefined -> ok;
        Action   -> run_action(Action, St), ok
    end.

%%--------------------------------------------------------------------
%% Actions
%%--------------------------------------------------------------------

run_action(Action, St) when is_atom(Action) ->
    Model = maps:get(model_m, St),
    Actions = maps:get(actions, Model, #{}),
    case maps:find(Action, Actions) of
        {ok, #{<<"type">> := <<"log">>}} ->
            Msg = maps:get(<<"message">>, Actions, Action),
            io:format("[STATEM ~s] ACTION ~s: ~s~n",
                      [maps:get(device, St), Action, Msg]);
        {ok, #{<<"type">> := <<"hook">>, <<"name">> := Hook}} ->
            dgiot_hook:run_hook(Hook, [St]);
        _ -> ok
    end;
run_action(_, _) -> ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

to_event_key(E) when is_atom(E) -> E;
to_event_key(E) when is_binary(E) -> binary_to_atom(E, utf8);
to_event_key(E) -> E.

log(DeviceId, Event, From, To) ->
    io:format("[STATEM ~s] ~p: ~p → ~p~n", [DeviceId, Event, From, To]).
