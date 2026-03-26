-module(test_magnetic_full_flow).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").

%% Import test_item record
-record(test_item, {
    id :: binary(),
    name :: binary(),
    station_id :: integer(),
    station_name :: binary(),
    steps = [] :: list(),
    order = 0 :: integer()
}).

-export([test/0]).

test() ->
    io:format("~n========================================~n", []),
    io:format("Magnetic Station Full Business Flow Test~n", []),
    io:format("========================================~n~n", []),

    %% Step 1: Check PLC Client
    io:format("Step 1: Check PLC Client for Magnetic Station (1700)~n", []),
    StationId = 1700,
    case global:whereis_name({plc, StationId}) of
        undefined ->
            io:format("  ERROR: PLC Client not found~n", []),
            {error, plc_not_found};
        PlcPid ->
            io:format("  PLC Client PID: ~p~n~n", [PlcPid]),

            %% Step 2: Load Test Items from Parse
            io:format("Step 2: Load Test Items from Parse~n", []),
            case dgiot_uav_test_loader:load_by_station(StationId) of
                {ok, TestItems} ->
                    io:format("  Loaded ~p test items~n~n", [length(TestItems)]),

                    %% Step 3: Display and Execute Test Items
                    io:format("Step 3: Test Items and PLC Commands~n", []),
                    execute_test_items(TestItems, PlcPid),

                    io:format("~n========================================~n", []),
                    io:format("Full Business Flow Test Complete~n", []),
                    io:format("========================================~n", []),
                    {ok, #{plc_pid => PlcPid, test_items => length(TestItems)}};

                {error, Reason} ->
                    io:format("  ERROR loading test items: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

execute_test_items([], _PlcPid) ->
    ok;
execute_test_items([Item | Rest], PlcPid) ->
    #test_item{
        id = ItemId,
        name = ItemName,
        steps = Steps
    } = Item,

    io:format("~n  Test Item: ~ts~n", [ItemName]),
    io:format("    ID: ~s~n", [ItemId]),
    io:format("    Steps: ~p~n", [length(Steps)]),

    %% Execute each step
    execute_steps(Steps, PlcPid),

    %% Process next test item
    execute_test_items(Rest, PlcPid).

execute_steps([], _PlcPid) ->
    ok;
execute_steps([Step | Rest], PlcPid) ->
    ActionType = maps:get(<<"action_type">>, Step, <<>>),
    Target = maps:get(<<"target">>, Step, <<>>),
    SendMap = maps:get(<<"send">>, Step, #{}),
    
    %% Extract content from send map
    SendValue = case SendMap of
        #{<<"content">> := Content} -> Content;
        _ -> <<>>
    end,

    io:format("    Step: action=~ts, target=~ts, send=~ts~n",
             [ActionType, Target, SendValue]),

    %% Send PLC command if action_type is "send" and target contains "PLC"
    case ActionType of
        <<"send">> ->
            io:format("      -> Send Action: target=~ts, content=~ts~n", [Target, SendValue]),
            case SendValue of
                <<>> ->
                    io:format("      -> Skipping empty send value~n", []);
                _ ->
                    try
                        Code = binary_to_integer(SendValue),
                        io:format("      -> Sending PLC Command: Code=~p~n", [Code]),
                        Result = dgiot_uav_plc_tcp_client:write(1700, 51, Code),
                        io:format("      -> Result: ~p~n", [Result]),
                        timer:sleep(1000)
                    catch
                        _:Error ->
                            io:format("      -> Error parsing code: ~p~n", [Error])
                    end
            end;
        <<"judge">> ->
            io:format("      -> Judge Action: waiting for sensor data~n", []);
        _ ->
            io:format("      -> Action: ~ts~n", [ActionType])
    end,

    execute_steps(Rest, PlcPid).
