# dgiot_task_worker.erl 消息处理分析

## 概述

`dgiot_task_worker.erl` 是一个gen_server模块，负责处理任务调度和设备指令发送。它实现了任务工作者的核心逻辑，包括定时任务触发、指令发送、ACK消息处理等。

## 消息处理类型分析

### 1. 系统消息处理

#### 1.1 EXIT消息
```erlang
handle_info({'EXIT', _From, Reason}, State) ->
    erlang:garbage_collect(self()),
    {stop, Reason, State};
```
**处理逻辑**：当收到EXIT消息时，进行垃圾回收并停止进程。

#### 1.2 stop消息
```erlang
handle_info(stop, #dclient{channel = Channel, client = Client} = Dclient) ->
    dgiot_client:stop(Channel, Client),
    erlang:garbage_collect(self()),
    {stop, normal, Dclient};
```
**处理逻辑**：停止客户端连接，进行垃圾回收，停止进程。

### 2. 任务调度消息

#### 2.1 change_clock消息
```erlang
handle_info({change_clock, NextTime, EndTime, Freq}, #dclient{clock = Clock} = Dclient) ->
    {noreply, Dclient#dclient{clock = Clock#dclock{nexttime = NextTime, count = dgiot_client:get_count(NextTime, EndTime, Freq), freq = Freq}}};
```
**处理逻辑**：动态修改任务启动时间、结束时间和执行频率。

#### 2.2 next_time消息（定时触发）
```erlang
handle_info(next_time, #dclient{channel = Channel, client = Client, userdata = UserData,
    clock = #dclock{round = Round, nexttime = NextTime, count = Count, freq = Freq} = Clock} = Dclient) ->
    dgiot_client:stop(Channel, Client, Count), %% 检查是否需要停止任务
    NewNextTime = dgiot_client:get_nexttime(NextTime, Freq),
    case dgiot_task:get_pnque(Client) of
        not_find ->
            {noreply, Dclient#dclient{clock = Clock#dclock{nexttime = NewNextTime, count = Count - 1}}};
        {ProductId, DevAddr} ->
            NewRound = Round + 1,
            DiQue = dgiot_task:get_instruct(ProductId, NewRound),
            PnQueLen = dgiot_task:get_pnque_len(Client),
            erlang:send_after(100, self(), read), % 每轮任务开始时，做一下随机开始
            {noreply, Dclient#dclient{userdata = UserData#device_task{product = ProductId, devaddr = DevAddr, pnque_len = PnQueLen, dique = DiQue},
                clock = Clock#dclock{nexttime = NewNextTime, count = Count - 1, round = NewRound}}}
    end;
```
**处理逻辑**：
1. 检查是否需要停止任务
2. 计算下一次执行时间
3. 获取产品ID和设备地址队列
4. 获取当前轮次的指令队列
5. 发送read消息开始执行指令

#### 2.3 read消息（发送指令）
```erlang
handle_info(read, State) ->
    {noreply, send_msg(State)};
```
**处理逻辑**：调用`send_msg/1`函数发送指令。

### 3. ACK消息处理

#### 3.1 dclient_ack消息（设备响应）
```erlang
handle_info({dclient_ack, Topic, Payload}, #dclient{channel = _ChannelId, userdata = Usedata} = State) ->
    dgiot_metrics:inc(dgiot_task, <<"task_recv">>, 1),
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"$dg">>, <<"thing">>, ProductId, DevAddr, <<"properties">>, <<"report">>] ->
            dgiot_task:save_td(ProductId, DevAddr, Payload, #{}),
            {noreply, send_msg(State#dclient{userdata = Usedata#device_task{product = ProductId, devaddr = DevAddr}})};
        _ ->
            io:format("~s ~p Topic = ~p.~n", [?FILE, ?LINE, Topic]),
            {noreply, send_msg(State)}
    end;
```
**处理逻辑**：
1. 增加接收计数器
2. 解析Topic，如果是设备属性上报消息：
   - 保存数据到TDengine
   - 继续发送下一条指令
3. 如果是其他Topic，记录日志并继续发送指令

### 4. 指令发送逻辑（send_msg函数）

#### 4.1 当前设备指令发送完成
```erlang
send_msg(#dclient{userdata = #device_task{dique = DisQue, pnque_len = PnQueLen} = UserData} = State) when length(DisQue) == 0 ->
    get_next_pn(State#dclient{userdata = UserData#device_task{pnque_len = PnQueLen - 1}});
```
**处理逻辑**：当前设备的指令队列为空时，切换到下一个设备。

#### 4.2 发送指令
```erlang
send_msg(#dclient{channel = ChannelId, clock = #dclock{freq = Freq}, userdata = #device_task{ref = Ref, product = Product, devaddr = DevAddr, dique = DisQue} = UserData} = State) ->
    {InstructOrder, Interval, _Identifier, _NewDataSource} = lists:nth(1, DisQue),
    {NewCount, _Payload, _Dis} =
        lists:foldl(fun(X, {Count, Acc, Acc1}) ->
            case X of
                {InstructOrder, _, Identifier1, DataSource} ->
                    Topic = <<"$dg/device/", Product/binary, "/", DevAddr/binary, "/properties">>,
                    Payload = dgiot_json:encode(DataSource#{<<"identifier">> => Identifier1, <<"_dgiotTaskFreq">> => Freq}),
                    dgiot_mqtt:publish(dgiot_utils:to_binary(ChannelId), Topic, Payload),
                    {Count + 1, Acc ++ [DataSource], Acc1 ++ [Identifier1]};
                _ ->
                    {Count, Acc, Acc1}
            end
                    end, {0, [], []}, DisQue),
    %%  在超时期限内，回报文，就取消超时定时器
    case Ref of
        undefined ->
            pass;
        _ -> erlang:cancel_timer(Ref)
    end,
    NewDisQue = lists:nthtail(NewCount, DisQue),
    dgiot_metrics:inc(dgiot_task, <<"task_send">>, 1),
    State#dclient{userdata = UserData#device_task{ref = erlang:send_after(Interval * 1000, self(), read), dique = NewDisQue, interval = Interval}}.
```
**处理逻辑**：
1. 获取第一条指令的指令序号和间隔
2. 遍历指令队列，发送相同指令序号的所有指令
3. 取消之前的定时器
4. 更新指令队列（移除已发送的指令）
5. 设置新的定时器，在指定间隔后发送下一条指令
6. 增加发送计数器

### 5. 设备切换逻辑（get_next_pn函数）

#### 5.1 本轮任务结束
```erlang
get_next_pn(#dclient{channel = ChannelId, clock = #dclock{round = Round}, userdata = #device_task{product = Product, devaddr = DevAddr, pnque_len = PnQueLen}} = State) when PnQueLen < 1 ->
    case PnQueLen of
        0 ->
            dgiot_bridge:send_log(dgiot_utils:to_binary(ChannelId), Product, DevAddr, "~s ~p time: ~p, round: ~p end ~n", [?FILE, ?LINE, dgiot_datetime:format(dgiot_datetime:now_secs(), <<"YY-MM-DD HH:NN:SS">>), Round]);
        _ ->
            pass
    end,
    State;
```
**处理逻辑**：当设备队列长度为0时，本轮任务结束，记录日志。

#### 5.2 切换到下一个设备
```erlang
get_next_pn(#dclient{client = CLient, clock = #dclock{round = Round}, userdata = #device_task{product = ProductId, devaddr = DevAddr} = UserData} = State) ->
    case dgiot_task:get_pnque(CLient) of
        not_find ->
            State;
        {ProductId, DevAddr} ->
            State;
        {NextProductId, NextDevAddr} ->
            DisQue = dgiot_task:get_instruct(NextProductId, Round),
            NewState = State#dclient{userdata = UserData#device_task{product = NextProductId, devaddr = NextDevAddr, dique = DisQue}},
            send_msg(NewState)
    end.
```
**处理逻辑**：
1. 获取下一个设备
2. 如果是同一个设备，保持状态
3. 如果是新设备，获取该设备的指令队列并开始发送

## 消息处理流程图

```
初始化
    ↓
等待消息
    ↓
┌─────────────────────────────────────────────┐
│             消息类型判断                     │
└─────────────────────────────────────────────┘
    │
    ├── EXIT消息 → 停止进程
    │
    ├── stop消息 → 停止客户端 → 停止进程
    │
    ├── change_clock消息 → 更新任务时间
    │
    ├── next_time消息 → 获取设备 → 发送read消息
    │
    ├── read消息 → send_msg → 发送指令
    │
    └── dclient_ack消息 → 保存数据 → send_msg
```

## 关键数据结构

### device_task记录
```erlang
-record(device_task, {
    ref = undefined,           % 定时器引用
    pnque_len = 0 :: integer(), % 本轮任务剩余的设备队列数
    product :: binary()|atom(), % 当前产品ID
    devaddr :: binary(),        % 当前设备地址
    dique = [] :: list(),       % 当前设备的指令队列
    interval = 3 :: integer(),  % 指令间隔（秒）
    appdata = #{} :: map()      % 用户自定义参数
}).
```

### dclient记录（来自dgiot_client.hrl）
```erlang
-record(dclient, {
    channel :: binary(),        % 通道ID
    client :: binary(),         % 客户端ID
    status :: atom(),           % 状态
    userdata :: any(),          % 用户数据（device_task记录）
    clock :: #dclock{}          % 时钟信息
}).
```

### dclock记录
```erlang
-record(dclock, {
    nexttime :: integer(),      % 下一次执行时间
    freq :: integer(),          % 执行频率（秒）
    count :: integer(),         % 剩余执行次数
    round :: integer()          % 当前轮次
}).
```

## 消息处理特点

### 1. 异步处理
- 所有消息处理都是异步的
- 使用定时器实现任务调度
- 支持并发处理多个设备

### 2. 状态管理
- 使用gen_server状态管理
- 状态包含设备信息、指令队列、定时器等
- 状态转换清晰

### 3. 错误处理
- EXIT消息处理确保进程清理
- 定时器取消防止内存泄漏
- 日志记录便于调试

### 4. 性能监控
- 使用dgiot_metrics统计任务发送和接收次数
- 日志记录任务执行情况
- 支持任务执行时间统计

## 使用场景

### 1. 定时数据采集
- 定时触发设备数据采集
- 支持错峰执行，避免并发过高

### 2. 设备控制
- 发送控制指令到设备
- 处理设备响应

### 3. 任务调度
- 支持多设备轮询
- 支持动态调整任务参数

## 注意事项

### 1. 定时器管理
- 每次发送指令后设置新的定时器
- 收到ACK消息时取消旧定时器
- 防止定时器泄漏

### 2. 状态一致性
- 确保状态更新原子性
- 避免竞态条件

### 3. 资源清理
- 进程停止时清理客户端连接
- 取消所有定时器
- 减少内存占用

## 总结

`dgiot_task_worker.erl` 实现了完整的任务调度和指令发送逻辑，支持：
1. 定时任务触发
2. 多设备轮询
3. 指令队列管理
4. 设备响应处理
5. 动态任务调整

消息处理逻辑清晰，状态管理完善，适合物联网设备管理和数据采集场景。
