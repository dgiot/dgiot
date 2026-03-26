-module(dgiot_uav_simple_plc_command).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

%% @doc 简化版PLC指令发送 - 扁平化设计,直接通过PID发送消息
%% 核心思想: 通过工位找到PID,直接发送消息,不经过任何gen_server调度器

%% API
-export([
    send_command/3,
    send_command/4,
    send_command/5,
    test/0
]).

%% @doc 发送PLC指令(3参数版本,调用5参数版本)
send_command(StationId, Addr, Value) ->
    send_command(StationId, Addr, Value, undefined, undefined).

%% @doc 发送PLC指令(4参数版本,调用5参数版本)
send_command(StationId, Addr, Value, TestItemId) ->
    send_command(StationId, Addr, Value, TestItemId, undefined).

%% @doc 发送PLC指令(5参数版本 - 主实现)
%% @param StationId 工位ID (如 1100, 1200, 1500)
%% @param Addr 寄存器地址
%% @param Value 寄存器值
%% @param TestItemId 测试项ID (可选)
%% @param StepIndex 步骤索引 (可选)
%% @returns {ok, Result} | {error, Reason}

send_command(StationId, Addr, Value, TestItemId, StepIndex) ->
    ?LOG(info, "[SIMPLE_PLC] 开始发送指令: 工位=~p, 地址=~p, 值=~p", [StationId, Addr, Value]),
    
    %% 步骤1: 通过工位查找PID
    case find_plc_pid(StationId) of
        {ok, Pid} ->
            ?LOG(info, "[SIMPLE_PLC] 步骤1成功: 找到PLC进程 ~p", [Pid]),
            
            %% 步骤2: 直接发送消息给PLC进程
            case send_to_plc_process(Pid, Addr, Value, TestItemId, StepIndex) of
                {ok, Result} ->
                    ?LOG(info, "[SIMPLE_PLC] 步骤2成功: 指令发送成功"),
                    {ok, Result};
                {error, Reason} ->
                    ?LOG(debug, "[SIMPLE_PLC] 步骤2失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(debug, "[SIMPLE_PLC] 步骤1失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 通过工位查找PLC进程PID
%% @param StationId 工位ID
%% @returns {ok, Pid} | {error, Reason}
find_plc_pid(StationId) ->
    %% 通过global注册查找PLC进程
    %% PLC进程在启动时会注册为 {plc, StationId}
    case global:whereis_name({plc, StationId}) of
        Pid when is_pid(Pid) ->
            ?LOG(info, "[SIMPLE_PLC] ✓ 找到PLC进程 - 工位: ~p, PID: ~p", [StationId, Pid]),
            {ok, Pid};
        undefined ->
            ?LOG(debug, "[SIMPLE_PLC] PLC进程不存在 - 工位: ~p", [StationId]),
            {error, plc_process_not_found}
    end.

%% @doc 直接发送消息给PLC进程
%% @param Pid PLC进程PID
%% @param Addr 寄存器地址
%% @param Value 寄存器值
%% @param TestItemId 测试项ID
%% @param StepIndex 步骤索引
%% @returns {ok, Result} | {error, Reason}
send_to_plc_process(Pid, Addr, Value, TestItemId, StepIndex) ->
    %% 检查进程是否存活
    case erlang:is_process_alive(Pid) of
        false ->
            ?LOG(debug, "[SIMPLE_PLC] PLC进程 ~p 已死亡", [Pid]),
            {error, process_dead};
        true ->
            ?LOG(info, "[SIMPLE_PLC] 发送消息到PLC进程: ~p", [Pid]),
            
            %% 直接发送消息 (不使用gen_server:call,避免阻塞)
            try
                Pid ! {simple_plc_command, Addr, Value, TestItemId, StepIndex, self()},
                
                %% 等待响应 (带超时)
                receive
                    {simple_plc_response, ok} ->
                        {ok, success};
                    {simple_plc_response, {error, Reason}} ->
                        {error, Reason}
                after 5000 ->
                    ?LOG(error, "[SIMPLE_PLC] 等待响应超时"),
                    {error, timeout}
                end
            catch
                Class:Exception:Stack ->
                    ?LOG(error, "[SIMPLE_PLC] 发送消息异常: ~p:~p~n堆栈: ~p", [Class, Exception, Stack]),
                    {error, {exception, Class, Exception}}
            end
    end.

%% @doc 测试函数
test() ->
    ?LOG(info, "========== 简化PLC指令模块测试 =========="),
    
    %% 测试工位1100
    case send_command(1100, 0, 1, <<"test">>, 1) of
        {ok, success} ->
            ?LOG(info, "✓ 测试通过: 工位1100指令发送成功"),
            ok;
        {error, Reason} ->
            ?LOG(error, "✗ 测试失败: ~p", [Reason]),
            {error, Reason}
    end.
