-module(prop_{{name}}).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0, initial_state_data/0,
         on/1, off/1, service/3, % State generators
         weight/3, precondition/4, postcondition/5, next_state_data/5]).

prop_test() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
        begin
            actual_system:start_link(),
            {History,State,Result} = proper_fsm:run_commands(?MODULE, Cmds), 
            actual_system:stop(),
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                [History,State,Result]),
                      aggregate(zip(proper_fsm:state_names(History),
                                    command_names(Cmds)), 
                                Result =:= ok))
        end).

-record(data, {}).

%% Initial state for the state machine
initial_state() -> on.
%% Initial model data at the start. Should be deterministic.
initial_state_data() -> #data{}.

%% State commands generation
on(_Data) -> [{off, {call, actual_system, some_call, [term(), term()]}}].

off(_Data) ->
    [{off, {call, actual_system, some_call, [term(), term()]}},
     {history, {call, actual_system, some_call, [term(), term()]}},
     { {service,sub,state}, {call, actual_system, some_call, [term()]}}].

service(_Sub, _State, _Data) ->
    [{on, {call, actual_system, some_call, [term(), term()]}}].

%% Optional callback, weight modification of transitions
weight(_FromState, _ToState, _Call) -> 1.

%% Picks whether a command should be valid. 
precondition(_From, _To, #data{}, {call, _Mod, _Fun, _Args}) -> true.

%% Given the state states and data *prior* to the call
%% `{call, Mod, Fun, Args}', determine if the result `Res' (coming
%% from the actual system) makes sense.
postcondition(_From, _To, _Data, {call, _Mod, _Fun, _Args}, _Res) -> true.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed. 
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    NewData = Data,
    NewData.
