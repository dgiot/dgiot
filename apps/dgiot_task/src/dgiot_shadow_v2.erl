%%--------------------------------------------------------------------
%% dgiot_shadow - IoT Device Shadow
%%
%% Shadow = Desired + Reported + Version + Delta
%%
%% Data flow:
%%   Cloud -> desired (MQTT: shadow/update)   -> device sync
%%   Device -> reported (MQTT: shadow/reported) -> cloud update
%%   delta = diff(desired, reported)          -> pending sync
%%   version++ on each successful sync
%%
%% Storage:
%%   Hot: ETS in-memory (microsecond read/write)
%%   Cold: PostgreSQL via dgiot_parse (persistent)
%%--------------------------------------------------------------------
-module(dgiot_shadow).
-author("dgaiot").
-behaviour(gen_server).

%% gen_server
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Shadow API
-export([
    get_shadow/1,
    get_shadow/2,
    to_map/1,
    update_desired/2,
    update_desired/3,
    update_reported/2,
    update_reported/3,
    get_delta/1,
    list_shadows/1,
    delete_shadow/1,
    sync_to_device/1,
    diff/2
]).

%% --- Record ---
-record(shadow, {
    device_id    :: binary(),
    product_id   :: binary(),
    desired = #{}:: map(),
    reported = #{}:: map(),
    version = 0  :: integer(),
    metadata = #{}:: map(),
    updated_at   :: integer()
}).

-type shadow() :: #shadow{}.

%% --- ETS ---
-define(SHADOW_TABLE, dgiot_shadow_cache).
-define(SHADOW_CLEANUP_MS, 3600000).  %% 1h

%% ===================================================================
%% gen_server
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?SHADOW_TABLE, [
        named_table, public, set,
        {keypos, #shadow.device_id},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    load_all_from_db(),
    subscribe_shadow_topics(),
    timer:send_interval(?SHADOW_CLEANUP_MS, cleanup_expired),
    logger:info("[shadow] manager started, ETS=~s", [?SHADOW_TABLE]),
    {ok, #{}}.

%% --- MQTT ---
handle_info({mqtt, Topic, Payload}, State) ->
    handle_shadow_mqtt(Topic, Payload),
    {noreply, State};

handle_info(cleanup_expired, State) ->
    Now = erlang:system_time(millisecond),
    SevenDays = 7 * 24 * 3600 * 1000,
    ets:foldl(fun(#shadow{device_id = Id, updated_at = Ts}, _) ->
        if Now - Ts > SevenDays -> ets:delete(?SHADOW_TABLE, Id);
           true -> ok
        end
    end, ok, ?SHADOW_TABLE),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --- Callbacks ---
handle_call({get_shadow, DeviceId}, _From, State) ->
    {reply, do_get_shadow(DeviceId), State};

handle_call({update_desired, DeviceId, Desired, Opts}, _From, State) ->
    {reply, do_update_desired(DeviceId, Desired, Opts), State};

handle_call({update_reported, DeviceId, Reported, Opts}, _From, State) ->
    {reply, do_update_reported(DeviceId, Reported, Opts), State};

handle_call({get_delta, DeviceId}, _From, State) ->
    {reply, do_get_delta(DeviceId), State};

handle_call({list_shadows, ProductId}, _From, State) ->
    {reply, do_list_shadows(ProductId), State};

handle_call({delete_shadow, DeviceId}, _From, State) ->
    {reply, do_delete_shadow(DeviceId), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:warning("[shadow] manager terminated: ~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Public API
%% ===================================================================

get_shadow(DeviceId) ->
    gen_server:call(?MODULE, {get_shadow, DeviceId}).

get_shadow(DeviceId, Default) ->
    case get_shadow(DeviceId) of
        {ok, Shadow} -> Shadow;
        {error, _} ->
            ProductId = dgiot_device:get_productid(DeviceId),
            #shadow{
                device_id = DeviceId, product_id = ProductId,
                desired = Default, reported = #{},
                version = 0, updated_at = erlang:system_time(millisecond)
            }
    end.

to_map(#shadow{device_id = Id, product_id = Pid,
               desired = D, reported = R,
               version = Ver, updated_at = Ts}) ->
    Delta = diff(D, R),
    #{
        <<"device_id">> => Id,
        <<"product_id">> => Pid,
        <<"desired">> => D,
        <<"reported">> => R,
        <<"delta">> => Delta,
        <<"version">> => Ver,
        <<"updated_at">> => Ts,
        <<"sync_status">> => case map_size(Delta) of 0 -> <<"synced">>; _ -> <<"pending">> end
    }.

update_desired(DeviceId, Desired) ->
    update_desired(DeviceId, Desired, #{}).

update_desired(DeviceId, Desired, Opts) ->
    gen_server:call(?MODULE, {update_desired, DeviceId, Desired, Opts}).

update_reported(DeviceId, Reported) ->
    update_reported(DeviceId, Reported, #{}).

update_reported(DeviceId, Reported, Opts) ->
    gen_server:call(?MODULE, {update_reported, DeviceId, Reported, Opts}).

get_delta(DeviceId) ->
    gen_server:call(?MODULE, {get_delta, DeviceId}).

list_shadows(ProductId) ->
    gen_server:call(?MODULE, {list_shadows, ProductId}).

delete_shadow(DeviceId) ->
    gen_server:call(?MODULE, {delete_shadow, DeviceId}).

sync_to_device(DeviceId) ->
    case get_shadow(DeviceId) of
        {ok, #shadow{desired = Desired, version = Ver}} ->
            Topic = iolist_to_binary([<<"$dg/things/">>, DeviceId, <<"/shadow/desired">>]),
            Payload = jsx:encode(#{<<"desired">> => Desired, <<"version">> => Ver}),
            dgiot_mqtt:publish(Topic, Payload, #{qos => 1, retain => true}),
            ok;
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% Internal - Core Logic
%% ===================================================================

do_get_shadow(DeviceId) ->
    case ets:lookup(?SHADOW_TABLE, DeviceId) of
        [#shadow{} = S] -> {ok, S};
        [] -> {error, not_found}
    end.

do_update_desired(DeviceId, Desired, _Opts) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(?SHADOW_TABLE, DeviceId) of
        [#shadow{version = Ver, reported = Reported} = S] ->
            NewVer = Ver + 1,
            NewShadow = S#shadow{
                desired = maps:merge(S#shadow.desired, Desired),
                version = NewVer, updated_at = Now
            },
            ets:insert(?SHADOW_TABLE, NewShadow),
            persist_shadow(NewShadow),
            publish_desired(DeviceId, NewShadow#shadow.desired, NewVer),
            Delta = diff(Desired, Reported),
            {ok, NewVer, Delta};
        [] ->
            ProductId = dgiot_device:get_productid(DeviceId),
            NewShadow = #shadow{
                device_id = DeviceId, product_id = ProductId,
                desired = Desired, reported = #{},
                version = 1, updated_at = Now
            },
            ets:insert(?SHADOW_TABLE, NewShadow),
            persist_shadow(NewShadow),
            publish_desired(DeviceId, Desired, 1),
            {ok, 1, Desired}
    end.

do_update_reported(DeviceId, Reported, _Opts) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(?SHADOW_TABLE, DeviceId) of
        [#shadow{desired = Desired, version = Ver} = S] ->
            MergedReported = maps:merge(S#shadow.reported, Reported),
            NewVer = Ver + 1,
            NewShadow = S#shadow{
                reported = MergedReported,
                version = NewVer, updated_at = Now
            },
            ets:insert(?SHADOW_TABLE, NewShadow),
            persist_shadow(NewShadow),
            Delta = diff(Desired, MergedReported),
            publish_delta(DeviceId, Delta, NewVer),
            {ok, NewVer, Delta};
        [] ->
            ProductId = dgiot_device:get_productid(DeviceId),
            NewShadow = #shadow{
                device_id = DeviceId, product_id = ProductId,
                desired = #{}, reported = Reported,
                version = 1, updated_at = Now
            },
            ets:insert(?SHADOW_TABLE, NewShadow),
            persist_shadow(NewShadow),
            {ok, 1, #{}}
    end.

do_get_delta(DeviceId) ->
    case ets:lookup(?SHADOW_TABLE, DeviceId) of
        [#shadow{desired = D, reported = R}] -> diff(D, R);
        [] -> #{}
    end.

do_list_shadows(ProductId) ->
    ets:foldl(fun(#shadow{product_id = Pid} = S, Acc) ->
        case Pid of ProductId -> [S | Acc]; _ -> Acc end
    end, [], ?SHADOW_TABLE).

do_delete_shadow(DeviceId) ->
    ets:delete(?SHADOW_TABLE, DeviceId),
    Topic = iolist_to_binary([<<"$dg/things/">>, DeviceId, <<"/shadow">>]),
    dgiot_mqtt:publish(Topic, <<>>, #{qos => 0, retain => true}),
    ok.

%% ===================================================================
%% Delta
%% ===================================================================

diff(Desired, Reported) ->
    Keys = sets:to_list(
        sets:union(
            sets:from_list(maps:keys(Desired)),
            sets:from_list(maps:keys(Reported)))),
    lists:foldl(fun(Key, Acc) ->
        D = maps:get(Key, Desired, undefined),
        R = maps:get(Key, Reported, undefined),
        if
            D =:= undefined -> Acc;
            R =:= undefined -> Acc#{Key => #{desired => D, reported => null}};
            D =:= R -> Acc;
            true -> Acc#{Key => #{desired => D, reported => R}}
        end
    end, #{}, Keys).

%% ===================================================================
%% Persistence
%% ===================================================================

persist_shadow(#shadow{device_id = Id, product_id = Pid,
                        desired = D, reported = R,
                        version = Ver, updated_at = Ts}) ->
    Doc = #{
        <<"device_id">> => Id, <<"product_id">> => Pid,
        <<"desired">> => jsx:encode(D), <<"reported">> => jsx:encode(R),
        <<"version">> => Ver, <<"updated_at">> => Ts
    },
    dgiot_parse:update_object(<<"DeviceShadow">>, Id, Doc).

load_all_from_db() ->
    case dgiot_parse:query_object(<<"DeviceShadow">>, #{}) of
        {ok, #{<<"results">> := Results}} when is_list(Results) ->
            lists:foreach(fun(#{<<"device_id">> := Id} = Doc) ->
                Shadow = #shadow{
                    device_id = Id,
                    product_id = maps:get(<<"product_id">>, Doc, <<>>),
                    desired = decode_json(maps:get(<<"desired">>, Doc, <<"{}">>)),
                    reported = decode_json(maps:get(<<"reported">>, Doc, <<"{}">>)),
                    version = maps:get(<<"version">>, Doc, 0),
                    updated_at = maps:get(<<"updated_at">>, Doc, 0)
                },
                ets:insert(?SHADOW_TABLE, Shadow)
            end, Results),
            logger:info("[shadow] loaded ~p shadows from db", [length(Results)]);
        _ ->
            logger:info("[shadow] no shadows in db, starting fresh", [])
    end.

%% ===================================================================
%% MQTT
%% ===================================================================

subscribe_shadow_topics() ->
    Topics = [
        <<"$dg/things/+/shadow/get">>,
        <<"$dg/things/+/shadow/reported">>,
        <<"$dg/things/+/shadow/desired">>
    ],
    lists:foreach(fun(T) -> dgiot_mqtt:subscribe(T, 1) end, Topics),
    logger:info("[shadow] subscribed to ~p topics", [length(Topics)]).

publish_desired(DeviceId, Desired, Version) ->
    Topic = iolist_to_binary([<<"$dg/things/">>, DeviceId, <<"/shadow/desired">>]),
    Payload = jsx:encode(#{
        <<"desired">> => Desired, <<"version">> => Version,
        <<"timestamp">> => erlang:system_time(millisecond)
    }),
    dgiot_mqtt:publish(Topic, Payload, #{qos => 1, retain => true}).

publish_delta(DeviceId, Delta, Version) ->
    case map_size(Delta) of
        0 -> ok;
        _ ->
            Topic = iolist_to_binary([<<"$dg/things/">>, DeviceId, <<"/shadow/delta">>]),
            Payload = jsx:encode(#{
                <<"delta">> => Delta, <<"version">> => Version,
                <<"timestamp">> => erlang:system_time(millisecond)
            }),
            dgiot_mqtt:publish(Topic, Payload, #{qos => 1})
    end.

handle_shadow_mqtt(Topic, Payload) ->
    Parts = binary:split(Topic, <<"/">>, [global]),
    case Parts of
        [<<"$dg">>, <<"things">>, DeviceId, <<"shadow">>, Action] ->
            handle_shadow_action(DeviceId, Action, Payload);
        _ ->
            logger:debug("[shadow] unknown topic: ~s", [Topic])
    end.

handle_shadow_action(DeviceId, <<"get">>, _Payload) ->
    sync_to_device(DeviceId);

handle_shadow_action(DeviceId, <<"reported">>, Payload) ->
    case jsx:decode(Payload, [return_maps]) of
        #{<<"reported">> := Reported} -> update_reported(DeviceId, Reported);
        _ -> update_reported(DeviceId, Payload)
    end;

handle_shadow_action(DeviceId, <<"desired">>, Payload) ->
    case jsx:decode(Payload, [return_maps]) of
        #{<<"desired">> := Desired} -> update_desired(DeviceId, Desired);
        _ -> update_desired(DeviceId, Payload)
    end;

handle_shadow_action(_DeviceId, _Action, _Payload) ->
    ok.

%% ===================================================================
%% Helpers
%% ===================================================================

decode_json(Bin) when is_binary(Bin) ->
    case jsx:decode(Bin, [return_maps]) of
        M when is_map(M) -> M;
        _ -> #{}
    end;
decode_json(_) -> #{}.
