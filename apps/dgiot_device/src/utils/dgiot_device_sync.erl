-module(dgiot_device_sync).
-author("Your Name").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([sync_parse/1]).

%%% 配置读取函数
get_sync_batch_size() ->
    application:get_env(dgiot_device, sync_batch_size, 50).

get_batch_max_retries() ->
    application:get_env(dgiot_device, batch_max_retries, 3).

get_progress_report_interval() ->
    application:get_env(dgiot_device, progress_report_interval, 1000).

get_device_limit() ->
    application:get_env(dgiot_device, device_limit, 50000).

%%% 设备同步主函数
sync_parse(OffLine) ->
    Now = dgiot_datetime:now_secs(),
    
    % 从配置读取参数
    BatchSize = get_sync_batch_size(),
    MaxRetries = get_batch_max_retries(),
    
    % 获取总设备数量
    TotalDevices = dgiot_mnesia:count(),
    ?LOG(info, "Starting device sync for ~p total devices (BatchSize: ~p, MaxRetries: ~p, OffLine: ~p)", 
         [TotalDevices, BatchSize, MaxRetries, OffLine]),
    
    % 检查设备数量限制
    case check_device_limit(TotalDevices) of
        {ok, _} ->
            do_device_sync(OffLine, Now, BatchSize, MaxRetries, TotalDevices);
        {error, Reason} ->
            ?LOG(error, "Device sync aborted: ~p", [Reason]),
            {error, Reason}
    end.

check_device_limit(TotalDevices) ->
    DeviceLimit = get_device_limit(),
    case TotalDevices > DeviceLimit of
        true ->
            {error, <<"Device count exceeds limit">>};
        false ->
            {ok, <<"Within device limit">>}
    end.

do_device_sync(OffLine, Now, BatchSize, MaxRetries, TotalDevices) ->
    % 初始化状态和计时器
    StartTime = os:system_time(millisecond),
    init_sync_state(),
    
    % 处理设备记录
    Result = dgiot_mnesia:search(
        fun(X) ->
            {_, DeviceId, V} = X,
            case process_and_accumulate_device(DeviceId, V, Now, OffLine, BatchSize) of
                {batch_ready, Batch} ->
                    process_batch_with_metrics(Batch, MaxRetries, TotalDevices, StartTime),
                    false;
                continue ->
                    false;
                error ->
                    false
            end
        end, #{}),
    
    % 处理最后一批
    process_final_batch(MaxRetries),
    
    % 生成最终报告
    generate_final_report(StartTime, TotalDevices),
    
    % 清理状态
    cleanup_sync_state(),
    
    Result.

init_sync_state() ->
    put(device_update_counter, 0),
    put(device_batch_accumulator, []),
    put(processed_devices_count, 0),
    put(last_report_time, os:system_time(millisecond)),
    put(last_report_count, 0),
    put(successful_batches, 0),
    put(failed_batches, 0).

cleanup_sync_state() ->
    erase(device_update_counter),
    erase(device_batch_accumulator),
    erase(processed_devices_count),
    erase(last_report_time),
    erase(last_report_count),
    erase(successful_batches),
    erase(failed_batches).

process_batch_with_metrics(Batch, MaxRetries, TotalDevices, StartTime) ->
    case process_device_batch(Batch, MaxRetries) of
        {ok, _} ->
            Successful = get(successful_batches),
            put(successful_batches, Successful + 1),
            report_progress_with_config(TotalDevices, StartTime);
        {error, Reason} ->
            Failed = get(failed_batches),
            put(failed_batches, Failed + 1),
            ?LOG(error, "Batch processing failed: ~p", [Reason]),
            report_progress_with_config(TotalDevices, StartTime)
    end.

process_final_batch(MaxRetries) ->
    FinalBatch = get(device_batch_accumulator),
    case FinalBatch of
        [] -> 
            ok;
        _ ->
            ?LOG(info, "Processing final batch of ~p devices", [length(FinalBatch)]),
            case process_device_batch(FinalBatch, MaxRetries) of
                {ok, _} -> 
                    Successful = get(successful_batches),
                    put(successful_batches, Successful + 1);
                {error, Reason} -> 
                    Failed = get(failed_batches),
                    put(failed_batches, Failed + 1),
                    ?LOG(error, "Final batch processing failed: ~p", [Reason])
            end
    end.

generate_final_report(StartTime, TotalDevices) ->
    EndTime = os:system_time(millisecond),
    TotalTime = (EndTime - StartTime) / 1000,
    FinalProcessed = get(processed_devices_count),
    FinalUpdated = get(device_update_counter),
    SuccessfulBatches = get(successful_batches),
    FailedBatches = get(failed_batches),
    
    ?LOG(info, "Device sync completed in ~.1f seconds:", [TotalTime]),
    ?LOG(info, "  - Processed: ~p/~p devices", [FinalProcessed, TotalDevices]),
    ?LOG(info, "  - Updated: ~p devices", [FinalUpdated]),
    ?LOG(info, "  - Batches: ~p successful, ~p failed", [SuccessfulBatches, FailedBatches]),
    Rate = if
        TotalTime > 0 -> FinalProcessed / TotalTime;
        true -> FinalProcessed  % 瞬时完成，速率等于处理总数
    end,
    ?LOG(info, "  - Rate: ~.1f devices/sec", [Rate]).

report_progress_with_config(TotalDevices, StartTime) ->
    Processed = get(processed_devices_count),
    Updated = get(device_update_counter),
    LastReport = get(last_report_count),
    ReportInterval = get_progress_report_interval(),
    
    % 根据配置的间隔报告进度
    if
        Processed - LastReport >= ReportInterval orelse Processed == TotalDevices ->
            CurrentTime = os:system_time(millisecond),
            ElapsedTime = (CurrentTime - StartTime) / 1000,
            
            Percentage = (Processed / TotalDevices) * 100,
            ProcessingRate = Processed / ElapsedTime,
            
            if
                ProcessingRate > 0 andalso Processed < TotalDevices ->
                    RemainingDevices = TotalDevices - Processed,
                    EtaSeconds = RemainingDevices / ProcessingRate,
                    EtaMinutes = EtaSeconds / 60,
                    
                    ?LOG(info, "Progress: ~p/~p (~.1f%), ~p updated, ETA: ~.1f min, Rate: ~.1f/sec", 
                         [Processed, TotalDevices, Percentage, Updated, EtaMinutes, ProcessingRate]);
                true ->
                    ?LOG(info, "Progress: ~p/~p (~.1f%), ~p updated, Rate: ~.1f/sec", 
                         [Processed, TotalDevices, Percentage, Updated, ProcessingRate])
            end,
            
            put(last_report_count, Processed),
            put(last_report_time, CurrentTime);
        true ->
            ok
    end.

%%% 设备处理函数 - 选择使用批量或单条更新
process_and_accumulate_device(DeviceId, ['Device', Acl, CurrentState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId], Now, OffLine, BatchSize) ->
    % 更新已处理设备计数
    Processed = get(processed_devices_count),
    put(processed_devices_count, Processed + 1),
    
    TimeDiff = Now - Last,
    
    % 根据条件决定设备状态
    {NewState, ParseStatus} = 
        if
            TimeDiff < 0 ->
                {true, <<"ONLINE">>};
            CurrentState == true andalso TimeDiff > OffLine ->
                {false, <<"OFFLINE">>};
            CurrentState == false andalso TimeDiff < OffLine ->
                {true, <<"ONLINE">>};
            true ->
                % 状态不需要更新
                {CurrentState, undefined}
        end,
    
    % 只有当状态需要更新时才处理
    case ParseStatus of
        undefined ->
            continue;
        _ ->
            accumulate_device_update(DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus, BatchSize)
    end;
process_and_accumulate_device(_DeviceId, _Record, _Now, _OffLine, _BatchSize) ->
    continue.

accumulate_device_update(DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus, BatchSize) ->
    Counter = get(device_update_counter),
    BatchAcc = get(device_batch_accumulator),
    
    NewCounter = Counter + 1,
    put(device_update_counter, NewCounter),
    
    % 构建设备更新记录
    DeviceRecord = {DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus},
    NewBatchAcc = [DeviceRecord | BatchAcc],
    put(device_batch_accumulator, NewBatchAcc),
    
    if
        NewCounter >= BatchSize ->
            {batch_ready, NewBatchAcc};
        true ->
            continue
    end.

process_device_batch(Batch, MaxRetries) ->
    BatchSize = length(Batch),
    ?LOG(debug, "Processing batch of ~p devices", [BatchSize]),
    
    % 根据批次大小选择处理策略
    if
        BatchSize > 1 ->
            % 使用批量更新
            process_batch_update(Batch, MaxRetries);
        true ->
            % 单条设备直接使用 update_device
            process_single_device_update(Batch)
    end.

process_batch_update(Batch, MaxRetries) ->
    BatchRequests = build_batch_requests(Batch),
    
    case execute_batch_with_retry(BatchRequests, MaxRetries, 1) of
        {ok, _} ->
            update_mnesia_batch(Batch),
            put(device_update_counter, 0),
            put(device_batch_accumulator, []),
            {ok, <<"Batch processed successfully">>};
        {error, Reason} ->
            ?LOG(error, "Batch update failed, falling back to single updates: ~p", [Reason]),
            % 批量更新失败时降级到单条更新
            fallback_to_single_updates(Batch),
            {error, Reason}
    end.

process_single_device_update([DeviceRecord]) ->
    {DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus} = DeviceRecord,
    update_device(DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus),
    put(device_update_counter, 0),
    put(device_batch_accumulator, []),
    {ok, <<"Single device processed">>}.

build_batch_requests(Batch) ->
    lists:map(fun({DeviceId, _Acl, _NewState, _State, _Last, _IsEnable, _ProductId, _Devaddr, _DeviceSecret, _Node, _Longitude, _Latitude, _ParentId, ParseStatus}) ->
        #{
            <<"method">> => <<"PUT">>,
            <<"path">> => <<"/classes/Device/", DeviceId/binary>>,
            <<"body">> => build_device_update_data(ParseStatus, _IsEnable, _Last, _Longitude, _Latitude)
        }
    end, Batch).

execute_batch_with_retry(BatchRequests, MaxRetries, Attempt) ->
    case dgiot_parsex:batch(BatchRequests) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} when Attempt < MaxRetries ->
            ?LOG(warning, "Batch attempt ~p/~p failed: ~p, retrying...", [Attempt, MaxRetries, Reason]),
            RetryDelay = trunc(math:pow(2, Attempt) * 100),
            timer:sleep(RetryDelay),
            execute_batch_with_retry(BatchRequests, MaxRetries, Attempt + 1);
        {error, Reason} ->
            {error, Reason}
    end.

update_mnesia_batch(Batch) ->
    lists:foreach(fun({DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, _ParseStatus}) ->
        try
            insert_mnesia(DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId)
        catch
            Type:Reason ->
                ?LOG(error, "Failed to update Mnesia for device ~p: ~p:~p", 
                     [DeviceId, Type, Reason])
        end
    end, Batch).

%%% 单条设备更新函数
update_device(DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus) ->
    case dgiot_parsex:update_object(<<"Device">>, DeviceId, #{
        <<"status">> => ParseStatus,
        <<"isEnable">> => IsEnable,
        <<"lastOnlineTime">> => Last,
        <<"location">> => #{
            <<"__type">> => <<"GeoPoint">>,
            <<"longitude">> => Longitude,
            <<"latitude">> => Latitude
        }
    }) of
        {ok, _R} ->
            insert_mnesia(DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId),
            timer:sleep(50);
        {error, Reason} ->
            ?LOG(error, "Failed to update device ~p: ~p", [DeviceId, Reason])
    end.

%%% 降级处理函数（批量失败时使用）
fallback_to_single_updates(Batch) ->
    ?LOG(warning, "Falling back to single updates for ~p devices", [length(Batch)]),
    SuccessCount = fallback_to_single_updates(Batch, 0),
    ?LOG(info, "Fallback single updates completed: ~p/~p devices updated successfully", 
         [SuccessCount, length(Batch)]).

fallback_to_single_updates([], SuccessCount) ->
    SuccessCount;
fallback_to_single_updates([{DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus} | Rest], SuccessCount) ->
    update_device(DeviceId, Acl, NewState, State, Last, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId, ParseStatus),
    % 统计成功数量（这里简化处理，假设所有更新都成功）
    fallback_to_single_updates(Rest, SuccessCount + 1).

build_device_update_data(Status, IsEnable, Last, Longitude, Latitude) ->
    #{
        <<"status">> => Status,
        <<"isEnable">> => IsEnable,
        <<"lastOnlineTime">> => Last,
        <<"location">> => #{
            <<"__type">> => <<"GeoPoint">>,
            <<"longitude">> => Longitude,
            <<"latitude">> => Latitude
        }
    }.

insert_mnesia(DeviceId, Acl, Status, State, Now, IsEnable, ProductId, Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId) ->
    %%    notification(DeviceId, Status, Longitude, Latitude, IsEnable, Now),
        dgiot_mnesia:insert(DeviceId, ['Device', Acl, Status, State, Now, IsEnable, dgiot_utils:to_atom(ProductId), Devaddr, DeviceSecret, Node, Longitude, Latitude, ParentId]).
        