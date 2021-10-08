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

-module(dgiot_metrics).
-include_lib("dgiot/include/dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([counter/2, counter/3, counter/4, gauge/2, gauge/3, gauge/4, summary/2, summary/3, summary/4, histogram/3, histogram/2, histogram/4]).
-export([counter_reset/1, counter_reset/2, counter_reset/3]).
-export([gauge_reset/1, gauge_reset/2, gauge_reset/3]).
-export([summary_reset/1, summary_reset/2, summary_reset/3]).
-export([histogram_reset/1, histogram_reset/2, histogram_reset/3]).

-export([init_metrics/1, collect_metrics/4]).
-export([start_metrics/1, inc/3, inc/4, inc/5, dec/3, dec/4, dec/5]).
-export([start/1, check_metrics/0]).

-route_path("/metrics/:Registry").
-export([init/2]).
init(Req0, ?MODULE) ->
    Registry = dgiot_req:binding(<<"Registry">>, Req0),
    Data = dgiot_stats:metrics(Registry),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, dgiot_utils:to_binary(Data), Req0),
    {ok, Req, ?MODULE}.

counter(Name, Value) when Value > 0 ->
    counter(Name, [], Value);
counter(Name, Value) when Value < 0 ->
    counter(Name, [], Value).
counter(Name, LabelValues, Value) ->
    counter(?DEFREGISTRY, Name, LabelValues, Value).
counter(Registry, Name, LabelValues, Value) when Value > 0 ->
    prometheus_counter:inc(Registry, Name, LabelValues, Value);
counter(Registry, Name, LabelValues, Value) when Value < 0 ->
    prometheus_counter:inc(Registry, Name, LabelValues, Value).

counter_reset(Name) ->
    counter_reset(Name, []).
counter_reset(Name, LabelValues) ->
    counter_reset(?DEFREGISTRY, Name, LabelValues).
counter_reset(Registry, Name, LabelValues) ->
    prometheus_counter:reset(Registry, Name, LabelValues).

gauge(Name, Value) ->
    gauge(Name, [], Value).
gauge(Name, LabelValues, Value) ->
    gauge(?DEFREGISTRY, Name, LabelValues, Value).
gauge(Registry, Name, LabelValues, Value) ->
    prometheus_gauge:set(Registry, Name, LabelValues, Value).


gauge_reset(Name) ->
    gauge_reset(Name, []).
gauge_reset(Name, LabelValues) ->
    gauge_reset(?DEFREGISTRY, Name, LabelValues).
gauge_reset(Registry, Name, LabelValues) ->
    prometheus_gauge:reset(Registry, Name, LabelValues).


summary(Name, Value) ->
    summary(Name, [], Value).
summary(Name, LabelValues, Value) ->
    summary(?DEFREGISTRY, Name, LabelValues, Value).
summary(Registry, Name, LabelValues, Value) ->
    prometheus_summary:observe(Registry, Name, LabelValues, Value).


summary_reset(Name) ->
    summary_reset(Name, []).
summary_reset(Name, LabelValues) ->
    summary_reset(?DEFREGISTRY, Name, LabelValues).
summary_reset(Registry, Name, LabelValues) ->
    prometheus_summary:reset(Registry, Name, LabelValues).


histogram(Name, Value) ->
    histogram(Name, [], Value).
histogram(Name, LabelValues, Value) ->
    histogram(?DEFREGISTRY, Name, LabelValues, Value).
histogram(Registry, Name, LabelValues, Value) ->
    prometheus_histogram:observe(Registry, Name, LabelValues, Value).


histogram_reset(Name) ->
    histogram_reset(Name, []).
histogram_reset(Name, LabelValues) ->
    histogram_reset(?DEFREGISTRY, Name, LabelValues).
histogram_reset(Registry, Name, LabelValues) ->
    prometheus_histogram:reset(Registry, Name, LabelValues).

%%新增统计函数
inc(Registry, Name, Value) ->
    {ok, Count} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, 0};
            {ok, Count1} -> {ok, Count1}
        end,
    dgiot_data:insert({Name, Registry}, Count + Value).

inc(Registry, Name, Value, Total, rate) ->
    {ok, Count} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, 0};
            {ok, Count1} -> {ok, Count1}
        end,
    case (Count + Value) >= Total of
        true ->
            dgiot_data:insert({Name, Registry}, 100);
        _ ->
            case Total > 0 of
                true -> dgiot_data:insert({Name, Registry}, round(100 * (Count + Value) / Total));
                _ -> dgiot_data:insert({Name, Registry}, 0)
            end
    end;

inc(Registry, Name, Value, Total, max) ->
    {ok, Count} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, 0};
            {ok, Count1} -> {ok, Count1}
        end,
    case (Count + Value) >= Total of
        true ->
            dgiot_data:insert({Name, Registry}, Total);
        _ ->
            dgiot_data:insert({Name, Registry}, (Count + Value))
    end.

inc(Registry, Name, Value, average) ->
    {ok, Count} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, 0};
            {ok, Count1} -> {ok, Count1}
        end,
    dgiot_data:insert({Name, Registry}, (Count + Value) / 2);

inc(Registry, Name, Label, Value) ->
    {ok, Map} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, #{}};
            {ok, Map1} -> {ok, Map1}
        end,
    Count = maps:get(Label, Map, 0),
    dgiot_data:insert({Name, Registry}, Map#{Label => Count + Value}).

dec(Registry, Name, Value) ->
    {ok, Count} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, 0};
            {ok, Count1} -> {ok, Count1}
        end,
    NewCount =
        case Count =< Value of
            true ->
                0;
            false ->
                Count - Value
        end,
    dgiot_data:insert({Name, Registry}, NewCount).

dec(Registry, Name, Value, Total, rate) ->
    {ok, Count} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, 0};
            {ok, Count1} -> {ok, Count1}
        end,
    NewCount =
        case Count > Value of
            true ->
                Count - Value;
            false ->
                0
        end,
    case Total > 0 of
        true ->
            dgiot_data:insert({Name, Registry}, round(100 * NewCount / Total));
        _ ->
            dgiot_data:insert({Name, Registry}, 0)
    end.

dec(Registry, Name, Label, Value) ->
    {ok, Map} =
        case dgiot_data:lookup({Name, Registry}) of
            {error, not_find} -> {ok, #{}};
            {ok, Map1} -> {ok, Map1}
        end,
    Count = maps:get(Label, Map, 0),
    NewCount =
        case Count > Value of
            true ->
                Count - Value;
            false ->
                0
        end,
    dgiot_data:insert({Name, Registry}, Map#{Label => NewCount}).

start(Registry) ->
    start_metrics(Registry).

start_metrics(Registry) ->
   dgiot_stats:new(Registry).

init_metrics(#{name := Name, registry := Registry, labels := Labels}) ->
    case Labels of
        [] ->
            dgiot_data:insert({Name, Registry}, 0);
        [#{<<"values">> := Values}] ->
            dgiot_data:insert({Name, Registry}, lists:foldl(fun(Label, Acc) -> Acc#{Label => 0} end, #{}, Values))
    end.


% Module =  ?MODULE,
collect_metrics(_Instance, Registry, Name, _Labels) ->
    {ok, Map} = dgiot_data:lookup({Name, Registry}),
    case is_map(Map) of
        true ->
            maps:fold(
                fun(Label, Value, Acc) ->
                    ok = gauge(Registry, Name, [Label], Value),
                    [Label | Acc]
                end, [], Map);
        false ->
            ok = gauge(Registry, Name, [], Map)
    end.

check_metrics() ->
    Fun =
        fun({App, _Desc, _Vsn}) ->
            case dgiot_plugin:check_module(App) of
                false ->
                    FileName = lists:concat([App, ".metrics"]),
                    case file:list_dir(code:priv_dir(App)) of
                        {ok, Files} ->
                            case lists:member(FileName, Files) of
                                true ->
                                    start(App);
                                false -> pass
                            end;
                        _ -> pass
                    end;
                _ -> pass
            end
        end,
    lists:map(Fun, application:loaded_applications()).
