%%%-------------------------------------------------------------------
%%% @doc UAV PLC 工具函数模块
%%% 提供地址映射、ID生成、命令列表规范化等辅助功能
%%%-------------------------------------------------------------------
-module(dgiot_uav_plc_utils).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").
-include("dgiot_uav_config.hrl").

%% API
-export([
    get_base_address/1,
    get_address_by_step/1,
    get_channel_id/0,
    get_client_id/1,
    normalize_command_list/1,
    test/0,
    test_client/0
]).

%%%===================================================================
%%% 地址映射
%%%===================================================================

%% @doc 获取工位的基地址
%% 支持两种输入：
%%   1. 工位ID (1-11) -> 返回对应的基地址
%%   2. 基地址 (1100-1700) -> 直接返回（已经是基地址）
-spec get_base_address(StationId :: integer()) -> integer().
get_base_address(StationId) when StationId >= 1000 ->
    %% 已经是基地址，直接返回
    StationId;
get_base_address(StationId) ->
    %% 工位ID映射到基地址
    case StationId of
        1 -> 1700;   %% 磁航向工位
        2 -> 1100;   %% 上料台/桁行架
        3 -> 1600;   %% 总测工位2
        4 -> 1600;   %% 总测工位2-动力检测
        5 -> 1500;   %% 总测工位1
        6 -> 1500;   %% 总测工位1-动力检测
        7 -> 1300;   %% 拷机工位2
        8 -> 1200;   %% 拷机工位1
        9 -> 1100;   %% 桁行架
        10 -> 5000;  %% 虚拟告警检测工位
        11 -> 5001;  %% 虚拟心跳检测工位
        _ -> StationId  %% 默认：使用工位ID作为基地址
    end.

%% @doc 获取步骤对应的相对地址
-spec get_address_by_step(StepId :: integer()) -> integer().
get_address_by_step(1) -> 0;
get_address_by_step(2) -> 51;
get_address_by_step(3) -> 10;
get_address_by_step(4) -> 0;
get_address_by_step(5) -> 10;
get_address_by_step(6) -> 60;
get_address_by_step(7) -> 61;
get_address_by_step(_) -> 0.

%%%===================================================================
%%% ID 生成
%%%===================================================================

get_channel_id() ->
    <<"e15d86fc34">>.

get_client_id(StationId) ->
    BinId = dgiot_utils:to_binary(StationId),
    <<"plc_", BinId/binary>>.

%%%===================================================================
%%% 命令列表规范化
%%%===================================================================

%% @doc 规范化命令列表，支持多种输入格式
%% 1. 整数 -> [{undefined, 整数}]
%% 2. 元组 {DeviceId, Code} -> [{DeviceId, Code}]
%% 3. 列表 -> 原样返回
%% 4. 其他 -> 空列表 []
-spec normalize_command_list(Value :: term()) -> list().
normalize_command_list(Value) when is_integer(Value) ->
    [{undefined, Value}];
normalize_command_list({DeviceId, Code}) when is_binary(DeviceId) orelse DeviceId =:= undefined ->
    [{DeviceId, Code}];
normalize_command_list(List) when is_list(List) ->
    List;
normalize_command_list(_) ->
    [].

%%%===================================================================
%%% 在线测试函数
%%%===================================================================

%% @doc 在线调试测试函数
%% @spec test() -> ok | {error, _Reason}
test() ->
    io:format("=== Testing DG-IoT UAV PLC Utils ===~n"),
    
    try
        %% 1. Test base address calculation
        io:format("~n1. Testing base address calculation:~n"),
        TestCases1 = [
            {1, 1700},
            {2, 1100},
            {3, 1600},
            {5, 1500},
            {10, 5000}
        ],
        
        lists:foreach(fun({StationId, Expected}) ->
            Actual = get_base_address(StationId),
            Status = case Actual == Expected of
                true -> "PASS";
                false -> "FAIL"
            end,
            io:format("  Station ~p: Expected=~p, Actual=~p [~s]~n", 
                [StationId, Expected, Actual, Status])
        end, TestCases1),
        
        %% 2. Test step address mapping
        io:format("~n2. Testing step address mapping:~n"),
        lists:foreach(fun(StepId) ->
            Addr = get_address_by_step(StepId),
            io:format("  Step ~p -> Address D~p~n", [StepId, Addr])
        end, [1,2,3,4,5,6,7]),
        
        %% 3. Test ID generation
        io:format("~n3. Testing ID generation:~n"),
        io:format("  Channel ID: ~s~n", [get_channel_id()]),
        io:format("  Client ID (1700): ~s~n", [get_client_id(1700)]),
        
        %% 4. Test command list normalization
        io:format("~n4. Testing command list normalization:~n"),
        NormCases = [
            {1, "integer"},
            {{undefined, 100}, "tuple"},
            {[{<<"dev1">>, 1}], "list"}
        ],
        
        lists:foreach(fun({Input, Desc}) ->
            Result = normalize_command_list(Input),
            io:format("  ~s: ~p -> ~p~n", [Desc, Input, Result])
        end, NormCases),
        
        io:format("~n=== Tests Completed ===~n"),
        ok
        
    catch
        Type:Error ->
            io:format("Test failed: ~p:~p~n", [Type, Error]),
            {error, Error}
    end.

%% @doc 测试PLC客户端主模块功能
%% @spec test_client() -> ok | {error, _Reason}
test_client() ->
    io:format("~n=== Testing PLC Client Module ===~n"),
    
    try
        %% 1. Check PLC process global names
        io:format("~n1. Checking PLC process registration:~n"),
        case global:registered_names() of
            Names when is_list(Names) ->
                PLCNames = [Name || Name <- Names, 
                    case Name of 
                        {plc, _StationId} -> true; 
                        _ -> false 
                    end],
                case PLCNames of
                    [] -> 
                        io:format("  No PLC processes found~n");
                    _ ->
                        io:format("  Found ~p PLC processes:~n", [length(PLCNames)]),
                        lists:foreach(fun({plc, StationId}) ->
                            case global:whereis_name({plc, StationId}) of
                                undefined -> 
                                    io:format("    Station ~p: process not found~n", [StationId]);
                                Pid -> 
                                    io:format("    Station ~p: process ~p~n", [StationId, Pid])
                            end
                        end, PLCNames)
                end;
            _ ->
                io:format("  Cannot get global name list~n")
        end,
        
        %% 2. Test station connection status
        io:format("~n2. Testing station connection status:~n"),
        TestStationId = 1,
        ChannelId = get_channel_id(),
        ClientId = get_client_id(TestStationId),
        
        io:format("  Channel: ~s, Client: ~s~n", [ChannelId, ClientId]),
        
        case dgiot_client:get(ChannelId, ClientId) of
            {ok, Pid} -> 
                io:format("  Station ~p process: ~p~n", [TestStationId, Pid]),
                
                %% Get connection status
                try gen_server:call(Pid, get_connection_status) of
                    {ok, Status} -> 
                        io:format("  Connection status: ~s~n", [Status]);
                    Error1 -> 
                        io:format("  Failed to get connection status: ~p~n", [Error1])
                catch
                    _:_ -> io:format("  Connection status check failed~n")
                end,
                
                %% Get test status
                try gen_server:call(Pid, get_continuous_test_status) of
                    {ok, TestStatus} -> 
                        io:format("  Test status: ~p~n", [TestStatus]);
                    {error, Reason} -> 
                        io:format("  Test status: ~p (not running)~n", [Reason])
                catch
                    _:_ -> io:format("  Test status check failed~n")
                end;
            _ -> 
                io:format("  Station ~p process not found or not started~n", [TestStationId])
        end,
        
        io:format("~n=== PLC Client Tests Completed ===~n"),
        ok
        
    catch
        Type:Error ->
            io:format("Client test failed: ~p:~p~n", [Type, Error]),
            {error, Error}
    end.