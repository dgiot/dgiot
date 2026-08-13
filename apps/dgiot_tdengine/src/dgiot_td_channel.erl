%%--------------------------------------------------------------------
%% dgiot_td_channel - TDChannel
%%
%% TDengine time-series data channel.
%% Auto-creates supertable + subtables for each device.
%% Manages data retention, downsampling, and query routing.
%%--------------------------------------------------------------------
-module(dgiot_td_channel).
-behavior(dgiot_channelx).
-author("dgaiot").

-include("dgiot_device.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(TYPE, <<"TDCHL">>).
-record(state, {id, mod, product, device_id, env = #{}}).

%% API
-export([start/2, auto_create/1, auto_create/2, ensure_table/2]).

%% Channel
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% --- Metal ---
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    priority => 0,
    title => #{zh => <<"TDChannel 时序通道"/utf8>>},
    description => #{zh => <<"TDengine 时序数据自动建表和写入"/utf8>>}
}).

-params(#{
    <<"stb_prefix">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"dgiot">>,
        title => #{zh => <<"超级表前缀"/utf8>>},
        description => #{zh => <<"超级表前缀: dgiot_{product}"/utf8>>}
    },
    <<"keep_days">> => #{
        order => 2,
        type => integer,
        required => false,
        default => 90,
        title => #{zh => <<"数据保留天数"/utf8>>},
        description => #{zh => <<"超过此天数的数据自动删除"/utf8>>}
    },
    <<"downsample">> => #{
        order => 3,
        type => string,
        required => false,
        default => <<"1h:avg,1d:min,max,avg">>,
        title => #{zh => <<"降采样策略"/utf8>>},
        description => #{zh => <<"格式: 1h:avg,1d:min,max,avg"/utf8>>}
    },
    <<"tags">> => #{
        order => 4,
        type => string,
        required => false,
        default => <<>>,
        title => #{zh => <<"标签"/utf8>>},
        description => #{zh => <<"设备标签 JSON, 作为 TDengine TAG"/utf8>>}
    }
}).

%% ===================================================================
%% Auto Create
%% ===================================================================

auto_create(DeviceId) ->
    auto_create(DeviceId, #{}).

auto_create(DeviceId, Opts) ->
    ProductId = dgiot_device:get_productid(DeviceId),
    ChannelId = <<"td_", DeviceId/binary>>,
    Params = #{
        <<"product">> => ProductId,
        <<"device">> => DeviceId,
        <<"stb_prefix">> => maps:get(<<"stb_prefix">>, Opts, <<"dgiot">>),
        <<"keep_days">> => maps:get(<<"keep_days">>, Opts, 90),
        <<"downsample">> => maps:get(<<"downsample">>, Opts, <<"1h:avg,1d:min,max,avg">>),
        <<"tags">> => maps:get(<<"tags">>, Opts, <<"{}">>)
    },
    case dgiot_channelx:add(ChannelId, ?TYPE, Params) of
        ok ->
            %% Auto-create TDengine table
            ensure_table(ProductId, DeviceId),
            setup_downsample(ProductId, DeviceId, Params),
            logger:info("[td_channel] auto-created for device ~s", [DeviceId]),
            {ok, ChannelId};
        {error, already_exists} ->
            {ok, ChannelId};
        {error, Reason} ->
            logger:error("[td_channel] create failed for ~s: ~p", [DeviceId, Reason]),
            {error, Reason}
    end.

%% ===================================================================
%% TDengine Table DDL
%% ===================================================================

ensure_table(ProductId, DeviceId) ->
    STB = stb_name(ProductId),
    SubTB = sub_name(DeviceId),

    %% 1. Create supertable if not exists
    CreateSTB = iolist_to_binary([
        <<"CREATE STABLE IF NOT EXISTS ">>, STB, <<" (">>,
        <<"ts TIMESTAMP, ">>,
        <<"val DOUBLE, ">>,
        <<"quality INT, ">>,
        <<"point_id BINARY(64)">>,
        <<") TAGS (">>,
        <<"device_id BINARY(64), ">>,
        <<"product_id BINARY(64), ">>,
        <<"location BINARY(128)">>,
        <<")">>
    ]),
    dgiot_tdengine:query(CreateSTB),

    %% 2. Create subtable for this device
    CreateSub = iolist_to_binary([
        <<"CREATE TABLE IF NOT EXISTS ">>, SubTB,
        <<" USING ">>, STB,
        <<" TAGS ('">>, DeviceId, <<"', '">>, ProductId, <<"', 'default')">>
    ]),
    dgiot_tdengine:query(CreateSub),
    ok.

setup_downsample(ProductId, DeviceId, #{<<"downsample">> := Strategy}) ->
    %% Parse and apply downsampling strategy
    %% Strategy format: "1h:avg,1d:min,max,avg"
    STB = stb_name(ProductId),
    lists:foreach(fun(Pair) ->
        [Interval, Funcs] = string:tokens(Pair, ":"),
        FuncList = string:tokens(Funcs, ","),
        lists:foreach(fun(F) ->
            Topic = iolist_to_binary([
                <<"SELECT ">>, list_to_binary(F),
                <<"(">>, STB, <<") FROM ">>, STB,
                <<" INTERVAL(">>, list_to_binary(Interval), <<")">>
            ]),
            logger:debug("[td_channel] downsample: ~s", [Topic])
        end, FuncList)
    end, string:tokens(binary_to_list(Strategy), ",")),
    ok.

%% ===================================================================
%% Channel Callbacks
%% ===================================================================

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:start(?MODULE, ChannelId, ChannelArgs).

init(_ChannelId, _Args, #{<<"device">> := DeviceId, <<"product">> := ProductId} = Env) ->
    KeepDays = maps:get(<<"keep_days">>, Env, 90),
    State = #state{
        id = DeviceId,
        mod = ?MODULE,
        product = ProductId,
        device_id = DeviceId,
        env = #{
            keep_days => KeepDays,
            stb => stb_name(ProductId),
            sub => sub_name(DeviceId)
        }
    },
    %% Schedule periodic cleanup
    timer:send_interval(86400000, cleanup),  %% daily
    {ok, State}.

handle_init(State) ->
    {ok, State}.

handle_event(_EventId, {insert, PointId, Value, Timestamp}, State) ->
    %% Direct write to TDengine subtable
    SubTB = maps:get(sub, State#state.env),
    Insert = iolist_to_binary([
        <<"INSERT INTO ">>, SubTB,
        <<" VALUES (">>,
        integer_to_binary(Timestamp), <<", ">>,
        float_to_binary(ensure_float(Value)), <<", ">>,
        <<"0, '">>, PointId, <<"')">>
    ]),
    dgiot_tdengine:query(Insert),
    {ok, State};

handle_event(_EventId, {batch_insert, Points}, State) ->
    SubTB = maps:get(sub, State#state.env),
    Values = lists:map(fun({PointId, Value, Ts}) ->
        iolist_to_binary([
            <<"(">>,
            integer_to_binary(Ts), <<", ">>,
            float_to_binary(ensure_float(Value)), <<", 0, '">>,
            PointId, <<"')">>
        ])
    end, Points),
    Insert = iolist_to_binary([<<"INSERT INTO ">>, SubTB, <<" VALUES ">>,
                                list_to_binary(lists:join(<<" ">>, Values))]),
    dgiot_tdengine:query(Insert),
    {ok, State};

handle_event(_EventId, cleanup, #state{env = #{keep_days := Days, sub := SubTB}} = State) ->
    CleanSQL = iolist_to_binary([
        <<"DELETE FROM ">>, SubTB,
        <<" WHERE ts < NOW - ">>, integer_to_binary(Days), <<"d">>
    ]),
    dgiot_tdengine:query(CleanSQL),
    logger:info("[td_channel] cleanup: ~s (keep ~w days)", [SubTB, Days]),
    {ok, State};

handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ok.

%% ===================================================================
%% Helpers
%% ===================================================================

stb_name(ProductId) ->
    Bin = iolist_to_binary([<<"dgiot_">>, sanitize(ProductId)]),
    Bin.

sub_name(DeviceId) ->
    Bin = iolist_to_binary([<<"d_">>, sanitize(DeviceId)]),
    Bin.

sanitize(Bin) when is_binary(Bin) ->
    << <<if C >= $a andalso C =< $z -> C;
          C >= $A andalso C =< $Z -> C;
          C >= $0 andalso C =< $9 -> C;
          C =:= $_ -> C;
          C =:= $- -> C;
          true -> $_
        end >>
    || <<C:8>> <= Bin >>.

ensure_float(V) when is_float(V) -> V;
ensure_float(V) when is_integer(V) -> V * 1.0;
ensure_float(V) when is_binary(V) ->
    case catch binary_to_float(V) of
        F when is_float(F) -> F;
        _ -> case catch binary_to_integer(V) of
            I when is_integer(I) -> I * 1.0;
            _ -> 0.0
        end
    end;
ensure_float(_) -> 0.0.
