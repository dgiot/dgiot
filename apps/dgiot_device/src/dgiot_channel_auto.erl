%%--------------------------------------------------------------------
%% dgiot_channel_auto - Auto-create channels on device registration
%%
%% Hook: after dgiot_device_manager:create_device/1
%% Creates: TaskChannel + TDChannel for each new device
%%--------------------------------------------------------------------
-module(dgiot_channel_auto).
-author("dgaiot").

-export([
    on_device_created/1,
    on_device_deleted/1,
    setup_all_channels/1,
    setup_all_channels/2,
    list_device_channels/1
]).

%% ===================================================================
%% Device lifecycle hooks
%% ===================================================================

%% Called after device is created. Auto-creates TaskChannel + TDChannel.
on_device_created(Device) ->
    DeviceId = maps:get(<<"devaddr">>, Device, maps:get(<<"device_id">>, Device)),
    case setup_all_channels(DeviceId, Device) of
        {ok, TaskCh, TDCh} ->
            logger:info("[channel_auto] device ~s: TaskChannel=~s, TDChannel=~s",
                        [DeviceId, TaskCh, TDCh]),
            {ok, #{task_channel => TaskCh, td_channel => TDCh}};
        {error, Reason} ->
            logger:error("[channel_auto] device ~s setup failed: ~p", [DeviceId, Reason]),
            {error, Reason}
    end.

%% Called before device is deleted. Removes channels.
on_device_deleted(Device) ->
    DeviceId = maps:get(<<"devaddr">>, Device, maps:get(<<"device_id">>, Device)),
    %% Remove TaskChannel
    catch dgiot_channelx:del(<<"task_", DeviceId/binary>>),
    %% Remove TDChannel
    catch dgiot_channelx:del(<<"td_", DeviceId/binary>>),
    logger:info("[channel_auto] channels removed for device ~s", [DeviceId]),
    ok.

%% ===================================================================
%% Setup
%% ===================================================================

setup_all_channels(DeviceId) ->
    setup_all_channels(DeviceId, #{}).

setup_all_channels(DeviceId, Opts) ->
    ProductId = dgiot_device:get_productid(DeviceId),

    %% Merge product-level channel config
    ProductOpts = get_product_channel_opts(ProductId),
    Merged = maps:merge(ProductOpts, Opts),

    %% 1. TDChannel first (table must exist before writes)
    TDCh = case dgiot_td_channel:auto_create(DeviceId, Merged) of
        {ok, ChId} -> ChId;
        {error, _} -> undefined
    end,

    %% 2. TaskChannel (depends on TD table for write stage)
    TaskCh = case dgiot_task_channel:auto_create(DeviceId, Merged#{<<"td_table">> => TDCh}) of
        {ok, ChId2} -> ChId2;
        {error, _} -> undefined
    end,

    %% 3. Subscribe device to shadow topic
    Topic = iolist_to_binary([<<"$dg/things/">>, DeviceId, <<"/shadow/#">>]),
    dgiot_mqtt:subscribe(Topic, 1),

    case {TaskCh, TDCh} of
        {undefined, _} -> {error, task_channel_failed};
        {_, undefined} -> {error, td_channel_failed};
        _ -> {ok, TaskCh, TDCh}
    end.

%% ===================================================================
%% Query
%% ===================================================================

list_device_channels(DeviceId) ->
    %% Query from channel registry
    case dgiot_channelx:list(DeviceId) of
        {ok, List} ->
            TaskChs = [C || C <- List, maps:get(<<"cType">>, C) =:= <<"TASKCHL">>],
            TDChs = [C || C <- List, maps:get(<<"cType">>, C) =:= <<"TDCHL">>],
            #{
                <<"device_id">> => DeviceId,
                <<"task_channels">> => TaskChs,
                <<"td_channels">> => TDChs,
                <<"total">> => length(List)
            };
        _ ->
            #{<<"device_id">> => DeviceId, <<"channels">> => [], <<"total">> => 0}
    end.

%% ===================================================================
%% Product-level defaults
%% ===================================================================

get_product_channel_opts(ProductId) ->
    case dgiot_product:get(ProductId) of
        {ok, Product} ->
            #{
                <<"batch_size">> => maps:get(<<"batch_size">>, Product, 100),
                <<"flush_interval">> => maps:get(<<"flush_interval">>, Product, 5000),
                <<"keep_days">> => maps:get(<<"keep_days">>, Product, 90),
                <<"downsample">> => maps:get(<<"downsample">>, Product, <<"1h:avg,1d:min,max,avg">>),
                <<"retry_max">> => maps:get(<<"retry_max">>, Product, 3)
            };
        _ ->
            #{
                <<"batch_size">> => 100,
                <<"flush_interval">> => 5000,
                <<"keep_days">> => 90,
                <<"retry_max">> => 3
            }
    end.
