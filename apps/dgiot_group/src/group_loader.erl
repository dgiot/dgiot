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

-module(group_loader).
-author("kenneth").
-include("dgiot_group.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

-define(dgiot_METRICS_METER, <<"dgiot_metrics_meter">>).
%% API
-export([start_link/0,
    init_td/0,
    start_tasks/0,
    start_task/1,
    start_schedule/1
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).


-define(PAGE_SIZE, 100).
-define(PAGE_INDEX, 1).
-define(MAX, 0).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
%%    dgiot_livequery:subscribe(<<"Crond">>, #{}),
    Topic = <<"shadow/productId/task">>,
    dgiot_utils:subscribe(Topic),
%%    load_task(),
    erlang:send_after(100, self(), init),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(init, State) ->
    erlang:send_after(1000, self(), load),
%%    dgiot_metrics:gauge(?GROUP_METRICS, ?dgiot_PARSE_METRICS, [<<"vcon">>], dgiot_group_utils:vcon()),
%%    dgiot_metrics:gauge(?GROUP_METRICS, ?dgiot_PARSE_METRICS, [<<"meter">>], dgiot_group_utils:meter()),
%%    dgiot_metrics:gauge(?GROUP_METRICS, ?dgiot_PARSE_METRICS, [<<"deveui">>], dgiot_group_utils:deveui()),
%%    dgiot_metrics:gauge(?GROUP_METRICS, ?dgiot_PARSE_METRICS, [<<"tq">>], dgiot_group_utils:tq()),
%%    dgiot_metrics:gauge(?GROUP_METRICS, ?dgiot_PARSE_METRICS, [<<"online_meter">>], dgiot_group_utils:online_meter()),
    {noreply, State};


handle_info(load_task, State) ->
%%    load_task(),
    {noreply, State};

handle_info(load, State) ->
    PageIndex = application:get_env(dgiot_group, page_index, ?PAGE_INDEX),
    PageSize = application:get_env(dgiot_group, page_size, ?PAGE_SIZE),
    Max = application:get_env(dgiot_group, vcon_total, ?MAX),
    Success = fun(Page) -> load_meter(Page) end,
    Query = #{<<"keys">> => [<<"vcaddr">>], <<"order">> => <<"-vcaddr">>},
    %?LOG(info,"~p",[{Query, PageIndex, PageSize, Max, Success}]),
    dgiot_parse_loader:start(<<"Vcon">>, Query, PageIndex, PageSize, Max, Success),
    {noreply, State};

%%handle_info({livequery, #{<<"object">> := Object} }, State) ->
%%    load_task(Object),
%%    {noreply, State};

%% MQTT接收
handle_info({deliver, _Topic, Msg}, State) ->
    _Payload = binary_to_term(dgiot_mqtt:get_payload(Msg)),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%load_task() ->
%%    QueryTask = #{<<"limit">> => 1000,
%%        <<"keys">> => [<<"active">>, <<"tid">>, <<"type">>, <<"crond">>, <<"task">>]},
%%    R = dgiot_parse:query_object(<<"Crond">>, QueryTask),
%%    case R of
%%        {ok, #{<<"results">> := Tasks}} ->
%%            ?LOG(info,"Tasks ~p ", [Tasks]),
%%            dgiot_data:insert(?dgiot_GROUP_TASK, <<"tasks">>, Tasks),
%%            lists:map(fun(X) ->
%%                case X of
%%                    #{<<"active">> := 1, <<"type">> := 4,
%%                        <<"task">> := #{<<"pns">> := <<"all">>}} = X ->
%%                        save_task(X);
%%                    _ ->
%%                        pass
%%                end end, Tasks);
%%        {error, Reason} ->
%%            ?LOG(error,"load task ~p", [Reason]),
%%            erlang:send_after(10000, self(), load_task)
%%    end.
%%
%%load_task(#{<<"active">> := 1, <<"type">> := 4,
%%    <<"task">> := Task, <<"tid">> := Tid, <<"crond">> := Crond}) ->
%%    save_task(#{<<"task">> => Task, <<"tid">> => Tid, <<"crond">> => Crond});

%%load_task(_Msg) ->
%%    pass.
%%
%%save_task(#{<<"task">> := Task, <<"tid">> := Tid, <<"crond">> := Crond}) ->
%%    Callback =
%%        fun(_X) ->
%%            group_loader:start_schedule(#{<<"tid">> => Tid, <<"task">> => Task})
%%        end,
%%    Id = list_to_binary(lists:concat(["dgiot_task/", integer_to_list(Tid, 10)])),
%%    #{<<"end_time">> := End_time, <<"start_time">> := Start_time} = Crond,
%%    ?LOG(info,"Start_time ~p End_time ~p ", [dgiot_datetime:to_localtime(Start_time),
%%        dgiot_datetime:to_localtime(End_time)]),
%%    ClockArgs = Crond#{<<"id">> => Id, <<"callback">> => Callback},
%%    #{<<"di">> := Dis} = Task,
%%
%%    lists:map(fun(Di) ->
%%        dgiot_mqtt:start_mqtt_workers(#{
%%            <<"tid">> => Tid,
%%            <<"di">> => Di,
%%            <<"threshold">> => 1000,
%%            <<"mod">> => group_schedule_worker,
%%            <<"fun">> => receive_from_mqtt})
%%              end, Dis),
%%    ?LOG(info,"ClockArgs ~p,", [ClockArgs]),
%%    dgiot_cron:save(default_task, ClockArgs);
%%
%%save_task(_) ->
%%    ok.

start_tasks() ->
    QueryTask = #{<<"limit">> => 1000,
        <<"keys">> => [<<"active">>, <<"tid">>, <<"type">>, <<"crond">>, <<"task">>]},
    R = dgiot_parse:query_object(<<"Crond">>, QueryTask),
    case R of
        {ok, #{<<"results">> := Tasks}} ->
            lists:map(fun(X) ->
                case X of
                    #{<<"active">> := 1, <<"type">> := 4, <<"tid">> := _Tid,
                        <<"task">> := #{<<"pns">> := <<"all">>}} = X ->
                        start_schedule(X);
                    _ ->
                        pass
                end
                      end, Tasks);
        _ -> pass
    end.

init_td() ->
    QueryTask = #{<<"limit">> => 1000,
        <<"keys">> => [<<"active">>, <<"tid">>, <<"type">>, <<"crond">>, <<"task">>]},
    R = dgiot_parse:query_object(<<"Crond">>, QueryTask),
    case R of
        {ok, #{<<"results">> := Tasks}} ->
            ?LOG(info,"Tasks ~p ", [Tasks]),
            lists:map(fun(X) ->
                case X of
                    #{<<"active">> := 1, <<"type">> := 4,
                        <<"task">> := #{<<"pns">> := <<"all">>}} = X ->
                        #{<<"task">> := #{<<"di">> := Dis}} = X,
                        lists:map(fun(Di) ->
                            dgiot_tdengine_bridge_dml_ddl:init_td(Di),
                            Fun1 =
                                fun({Key, _Value}, _) ->
%%                                    case Key of
%%                                        VcAddr ->
                                            timer:sleep(5),
                                            dgiot_tdengine_bridge_dml_ddl:create_tables(Di, Key),
                                            false
%%                                        _ -> false
%%                                    end
                                end,
                            dgiot_data:search(?dgiot_GROUP, Fun1)
                                  end, Dis);
                    _ ->
                        pass
                end end, Tasks);
        _ -> pass
    end.

start_schedule(#{<<"tid">> := Tid, <<"task">> := Task}) ->
    Chs = maps:get(<<"chs">>, Task, []),
    case Chs of
        [] ->
            ?LOG(error,"ch is null Tid ~p , Task ~p", [Tid, Task]),
            pass;
        _ ->
            dgiot_data:insert(?dgiot_GROUP_TASK, {<<"Chs">>, Tid}, Chs),
            Res = maps:get(<<"res">>, Task, []),
            dgiot_data:insert(?dgiot_GROUP_TASK, {<<"Res">>, Tid}, Res),
            Dis = maps:get(<<"di">>, Task),
            Pns = maps:get(<<"pns">>, Task),
            lists:map(fun(Di) ->
                lists:foldl(
                    fun([Cid, _], Sum) ->
                        Rid = 1 + Sum,
                        ?LOG(info,"Di ~p ,Tid ~p , Rid ~p , Cid ~p", [Di, Tid, Rid, Cid]),
                        dgiot_metrics:counter_reset(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Cid, "total"]),
                        dgiot_metrics:counter_reset(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Cid, "to_vcon"]),
                        dgiot_metrics:counter_reset(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Cid, "to_meter"]),
                        dgiot_metrics:counter_reset(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Cid, "to_msg"]),
                        dgiot_metrics:counter_reset(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Cid, "to_td"]),
                        dgiot_metrics:counter_reset(?GROUP_METRICS, ?dgiot_GROUP_METRICS, [Di, Tid, Rid, Cid, "to_task"]),
                        Rid
                    end, 0, Chs),
                dgiot_data:set_consumer({<<"taskdelay">>, Tid, Di}, 1000000),
                Fun =
                    fun({Key, _Value}, _) ->
%%                        case Key of
%%                            VcAddr ->
                                Fdate = maps:get(<<"fdate">>, Task, dgiot_datetime:nowstamp()),
                                Args = #{
                                    <<"di">> => Di,
                                    <<"fdate">> => Fdate,
                                    <<"vcaddr">> => Key,
                                    <<"tid">> => Tid,
                                    <<"pns">> => Pns},
                                supervisor:start_child(group_schedule, [Args]),
                                false
%%                            _ -> false
%%                        end
                    end,
                dgiot_data:search(?dgiot_GROUP, Fun)
                      end, Dis)
    end.

load_meter([]) -> ok;
load_meter([#{<<"vcaddr">> := VcAddr} = Group | Groups]) ->
    case catch query_meter(Group) of
        ok ->
            load_meter(Groups);
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error,"Load Meter error,~p, ~p", [VcAddr, Reason]),
            {error, Reason}
    end.

query_meter(#{<<"vcaddr">> := VcAddr} = Group) ->
    Query = #{
        <<"where">> => #{<<"vcaddr">> => VcAddr},
        <<"limit">> => 600,
        <<"order">> => <<"pn">>,
        <<"keys">> => [<<"pn">>, <<"addr">>, <<"channel">>]
    },
    case dgiot_parse:query_object(<<"Smartmeter">>, Query) of
        {ok, #{<<"results">> := Meters}} ->
            dgiot_group:start_group(Group#{<<"meters">> => Meters}),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

start_task(#{<<"mid">> := _Mid,
    <<"tid">> := _Tid,
    <<"di">> := _Di,
    <<"mod">> := _Mod,
    <<"fun">> := _Fun,
    <<"que">> := _Que,
    <<"quelen">> := _QueLen,
    <<"data">> := _Tags,
    <<"delay">> := _Delay,
    <<"retry">> := _Rerty,
    <<"times">> := _Times,
    <<"begin">> := _Begin} = Args) ->
    supervisor:start_child(group_task, [Args]).
