%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_stats).

-behaviour(gen_server).

-include("dgiot.hrl").
-include("logger.hrl").
-include("types.hrl").

-logger_header("[Stats]").

%% APIs
-export([start_link/0
    , start_link/1
    , stop/0
]).


%% Stats API.
-export([getstats/0
    , getstat/1
    , setstat/2
    , setstat/3
    , statsfun/1
    , statsfun/2
]).

-export([update_interval/2
    , update_interval/3
    , cancel_update/1
]).

-export([query_range/5
    , new/1
    , new/2
    , metrics/1
    , metrics/2]).

%% gen_server callbacks
-export([init/1
    , handle_call/3
    , handle_cast/2
    , handle_info/2
    , terminate/2
    , code_change/3
]).

-export_type([stats/0]).

-define(TIMER_MSG, '#interval').

%% Channel stats
-define(CHANNEL_STATS,
    ['channels.count', %% Count of Concurrent Channels
        'channels.max'    %% Maximum Number of Concurrent Channels
    ]).

%% Route stats
-define(MNESIA_STATS,
    ['mnesia.count',
        'mnesia.max'
    ]).


-record(update, {name, countdown, interval, func}).

-record(state, {
    timer :: maybe(reference()),
    updates :: [#update{}],
    tick_ms :: timeout(),
    instances,
    collectors = []
}).

-type(stats() :: list({atom(), non_neg_integer()})).

-define(TAB, ?MODULE).
-define(SERVER, ?MODULE).

-type(opts() :: #{tick_ms := timeout()}).

%% @doc Start stats server
-spec(start_link() -> startlink_ret()).
start_link() ->
    start_link(#{tick_ms => timer:seconds(1)}).

-spec(start_link(opts()) -> startlink_ret()).
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

-spec(stop() -> ok).
stop() ->
    gen_server:call(?SERVER, stop, infinity).

%% @doc Generate stats fun.
-spec(statsfun(Stat :: atom()) -> fun()).
statsfun(Stat) ->
    fun(Val) -> setstat(Stat, Val) end.

-spec(statsfun(Stat :: atom(), MaxStat :: atom()) -> fun()).
statsfun(Stat, MaxStat) ->
    fun(Val) -> setstat(Stat, MaxStat, Val) end.

%% @doc Get all statistics.
-spec(getstats() -> stats()).
getstats() ->
    case ets:info(?TAB, name) of
        undefined -> [];
        _ -> ets:tab2list(?TAB)
    end.

%% @doc Get stats by name.
-spec(getstat(atom()) -> maybe(non_neg_integer())).
getstat(Name) ->
    case ets:lookup(?TAB, Name) of
        [{Name, Val}] -> Val;
        [] -> undefined
    end.

%% @doc Set stats
-spec(setstat(Stat :: atom(), Val :: pos_integer()) -> boolean()).
setstat(Stat, Val) when is_integer(Val) ->
    safe_update_element(Stat, Val).

%% @doc Set stats with max value.
-spec(setstat(Stat :: atom(), MaxStat :: atom(),
    Val :: pos_integer()) -> ok).
setstat(Stat, MaxStat, Val) when is_integer(Val) ->
    cast({setstat, Stat, MaxStat, Val}).

-spec(update_interval(atom(), fun()) -> ok).
update_interval(Name, UpFun) ->
    update_interval(Name, 1, UpFun).

-spec(update_interval(atom(), pos_integer(), fun()) -> ok).
update_interval(Name, Secs, UpFun) when is_integer(Secs), Secs >= 1 ->
    cast({update_interval, rec(Name, Secs, UpFun)}).

-spec(cancel_update(atom()) -> ok).
cancel_update(Name) ->
    cast({cancel_update, Name}).

rec(Name, Secs, UpFun) ->
    #update{name = Name, countdown = Secs, interval = Secs, func = UpFun}.

cast(Msg) -> gen_server:cast(?SERVER, Msg).

new(Registry) ->
    new(Registry, []).

new(Registry, Collectors) ->
    case load_config(lists:concat([code:priv_dir(Registry), "/", Registry, ".metrics"])) of
        {ok, Metrics} ->
            Instance = get_instance(),
            gen_server:call(?MODULE, {add, Instance, Registry, Metrics, Collectors}, infinity);
        _ -> pass
    end.

query_range(Version, Query, Start, End, Step) ->
    case dgiot:get_env(prometheus_server) of
        {ok, Host} ->
            Q = dgiot_httpc:urlencode(Query),
            Path = binary_to_list(<<"/api/", Version/binary, "/query_range?query=", Q/binary>>),
            Url = lists:concat([Host, Path, "&start=", Start, "&end=", End, "&step=", Step]),
            case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
                {ok, {{_HTTPVersion, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
                    case catch jsx:decode(Body, [{labels, binary}, return_maps]) of
                        {'EXIT', Reason} ->
                            {error, Reason};
                        Data ->
                            {ok, StatusCode, Data}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        undefined ->
            {error, prometheus_not_find}
    end.

metrics(Registry) ->
    {ok, Instances} = gen_server:call(?MODULE, get_instances, 30000),
    metrics(Instances, dgiot_utils:to_atom(Registry), get_instance()).


metrics(Fun, Acc0) ->
    {ok, Instances} = gen_server:call(?MODULE, get_instances, 30000),
    lists:foldl(
        fun({Registry, Instance, Metrics}, Acc1) ->
            Data = metrics([{Registry, Instance, Metrics}], Registry, Instance),
            Fun({Registry, Instance, Data}, Acc1)
        end, Acc0, Instances).

metrics(Instances, Registry, Instance) ->
    lists:map(fun(X) ->
        case X of
            {Registry, Instance, Metrics} ->
                [dgiot_metrics:collect_metrics(Instance, Registry, Name, Labels) || #{name := Name, labels := Labels} <- Metrics];
            _ ->
                ok
        end
              end, Instances),
    prometheus_text_format:format(Registry).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(#{tick_ms := TickMs}) ->
    Collectors = dgiot:get_env(collectors, []),
    ok = dgiot_tables:new(?TAB, [public, set, {write_concurrency, true}]),
    Stats = lists:append([
        ?CHANNEL_STATS,
        ?MNESIA_STATS
    ]),
    true = ets:insert(?TAB, [{Name, 0} || Name <- Stats]),
    {ok, start_timer(#state{instances = [], collectors = Collectors, updates = [], tick_ms = TickMs}), hibernate}.

start_timer(#state{tick_ms = Ms} = State) ->
    State#state{timer = dgiot_misc:start_timer(Ms, tick)}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({add, Instance, Registry, Metrics, Collectors}, _From, S = #state{instances = Instances}) ->
    ok = prometheus_registry:register_collectors(Registry, S#state.collectors ++ Collectors),
    NewMetrics =
        lists:foldl(
            fun(Info, Acc) ->
                case add(Registry, Info) of
                    {error, mf_already_exists} ->
                        Acc;
                    {error, Reason} ->
                        ?LOG(warning, "add metrics error, ~p,~p", [Info, Reason]),
                        Acc;
                    Spec ->
                        ?LOG(info, "add metrics success, ~p", [Spec]),
                        [Spec | Acc]
                end
            end, [], Metrics),
    {reply, ok, S#state{instances = [{Registry, Instance, NewMetrics} | Instances]}};

handle_call(get_instances, _From, #state{instances = Instances} = State) ->
    {reply, {ok, Instances}, State};

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Req]),
    {reply, ignored, State}.

handle_cast({setstat, Stat, MaxStat, Val}, State) ->
    try ets:lookup_element(?TAB, MaxStat, 2) of
        MaxVal when Val > MaxVal ->
            ets:update_element(?TAB, MaxStat, {2, Val});
        _ -> ok
    catch
        error:badarg ->
            ets:insert(?TAB, {MaxStat, Val})
    end,
    safe_update_element(Stat, Val),
    {noreply, State};

handle_cast({update_interval, Update = #update{name = Name}},
    State = #state{updates = Updates}) ->
    NState = case lists:keyfind(Name, #update.name, Updates) of
                 #update{} ->
                     ?LOG(warning, "Duplicated update: ~s", [Name]),
                     State;
                 false -> State#state{updates = [Update | Updates]}
             end,
    {noreply, NState};

handle_cast({cancel_update, Name}, State = #state{updates = Updates}) ->
    Updates1 = lists:keydelete(Name, #update.name, Updates),
    {noreply, State#state{updates = Updates1}};

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({timeout, TRef, tick}, State = #state{timer = TRef, updates = Updates}) ->
    Updates1 = lists:foldl(
        fun(Update = #update{name = Name, countdown = C, interval = I,
            func = UpFun}, Acc) when C =< 0 ->
            try UpFun()
            catch
                _:Error ->
                    ?LOG(error, "Update ~s failed: ~0p", [Name, Error])
            end,
            [Update#update{countdown = I} | Acc];
            (Update = #update{countdown = C}, Acc) ->
                [Update#update{countdown = C - 1} | Acc]
        end, [], Updates),
    {noreply, start_timer(State#state{updates = Updates1}), hibernate};

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{timer = TRef}) ->
    dgiot_misc:cancel_timer(TRef).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

safe_update_element(Key, Val) ->
    try ets:update_element(?TAB, Key, {2, Val}) of
        false ->
            ets:insert_new(?TAB, {Key, Val});
        true -> true
    catch
        error:badarg ->
            ?LOG(warning, "Failed to update ~0p to ~0p", [Key, Val])
    end.

add(Registry, Stat) ->
    case catch new_registry(Registry, Stat) of
        {'EXIT', {{mf_already_exists, _, _}, _}} ->
            {error, mf_already_exists};
        {'EXIT', {{mf_already_exists, _Reason}, _}} ->
            {error, mf_already_exists};
        {'EXIT', Reason} ->
            ?LOG(info, "~p~n", [Reason]),
            {error, Reason};
        Spec -> Spec
    end.


new_registry(Registry, #{<<"type">> := Type, <<"name">> := Name, <<"help">> := Help} = Stat) ->
    StrHelp = unicode:characters_to_list(Help),
    Labels = maps:get(<<"labels">>, Stat, []),
    Spec = #{
        name => Name,
        help => StrHelp,
        registry => Registry,
        labels => [Label || #{<<"label">> := Label} <- Labels]
    },
    case Type of
        <<"histogram">> ->
            #{<<"bounds">> := Bounds} = Stat,
            collect_metrics(<<"histogram">>, Labels, Spec#{bounds => Bounds});
        _ ->
            collect_metrics(Type, Labels, Spec)
    end.

% 只增不减 计数器
collect_metrics(<<"counter">>, Labels, Spec) ->
    prometheus_counter:new(maps:to_list(Spec)),
    do_callback(Spec#{labels => Labels});
% 可增可减 仪表盘
collect_metrics(<<"gauge">>, Labels, Spec) ->
    prometheus_gauge:new(maps:to_list(Spec)),
    do_callback(Spec#{labels => Labels});
% 直方图
collect_metrics(<<"histogram">>, Labels, Spec) ->
    prometheus_histogram:new(maps:to_list(Spec)),
    do_callback(Spec#{labels => Labels});
% 摘要型
collect_metrics(<<"summary">>, Labels, Spec) ->
    prometheus_summary:new(maps:to_list(Spec)),
    do_callback(Spec#{labels => Labels}).

do_callback(Spec) ->
    apply(dgiot_metrics, init_metrics, [Spec]),
    maps:without([help, registry], Spec).

load_config(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, jiffy:decode(Bin, [return_maps])};
        _ ->
            {error, <<"readfile failed">>}
    end.

get_instance() ->
    [_Name, Instance] = string:tokens(atom_to_list(node()), "@"),
    Instance.

%%{
%%"ID": "node-exporter",
%%"Name": "node-exporter-172.30.12.167",
%%"Tags": [
%%"test"
%%],
%%"Address": "172.30.12.167",
%%"Port": 9100,
%%"Meta": {
%%"app": "spring-boot",
%%"team": "appgroup",
%%"project": "bigdata"
%%},
%%"EnableTagOverride": false,
%%"Check": {
%%"HTTP": "http://172.30.12.167:9100/metrics",
%%"Interval": "10s"
%%},
%%"Weights": {
%%"Passing": 10,
%%"Warning": 1
%%}
%%}

%%./consul agent -server -bootstrap-expect 1 -data-dir=/tmp/consul -node=n1 -bind=127.0.0.1 -client=0.0.0.0 -ui

%% curl -X PUT -d '{"id": "node-exporter","name": "node-exporter-172.30.12.167","address": "172.30.12.167","port": 9100,"tags": ["test"],"checks": [{"http": "http://172.30.12.167:9100/metrics", "interval": "5s"}]}'  http://127.0.0.1:8500/v1/agent/service/register
%%post_consul() ->
%%    ok.

%% curl -X PUT http://127.0.0.1:8500/v1/agent/service/deregister/node-exporter
%%delete_consul() ->
%%    ok.

%% 动态添加面板
%%curl -i -X POST -H "Authorization: Bearer your-api-key" -H "Accept: application/json" -H "Content-Type: application/json" -k -v  http://your_grafana:3000/api/dashboards/db -d '{ "dashboard": { "id": null, "title": "api-test", "tags": [ "templated" ], "timezone": "browser", "rows": [ { } ], "schemaVersion": 6, "version": 0 }, "overwrite": false }'
