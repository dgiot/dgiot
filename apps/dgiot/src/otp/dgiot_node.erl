%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%  emqx_mgmt_cli:cluster(["join", "dgiot_iot@115.159.144.54"])
%%%  ekka:join(ekka_node:parse_name("dgiot_iot@115.159.144.54"))
%%%  [[{emqx_mgmt_cli,cluster}]]
%%% @end
%%% Created : 20. 十二月 2018 22:27
%%%-------------------------------------------------------------------
-module(dgiot_node).
-author("kenneth").
-include("dgiot.hrl").
-include_lib("logger.hrl").
-behaviour(gen_server).
-export([start_link/0, get_info/0, get_memory/0, get_cpu/0, get_ports/0, join/1, leave/1, get_nodes/0, get_nodes/1, choose_node/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([shutdown/1, reboot/0]).
-record(state, {  }).

-define(UTIL_ALLOCATORS, [temp_alloc, eheap_alloc, binary_alloc, ets_alloc, driver_alloc, sl_alloc, ll_alloc, fix_alloc, std_alloc]).

join(Node) when is_binary(Node) ->
    join(binary_to_list(Node));
join(Node) when is_list(Node) ->
    join(ekka_node:parse_name(Node));
join(Node) when is_atom(Node) ->
    %ekka_callback(prepare),
    %ekka_callback(reboot),
    case ekka:join(Node) of
        ok -> ok;
        ignore -> {error, ignore};
        {error, {Reason, Node}} -> {error, Reason}
    end.


leave(Node) when is_binary(Node) ->
    leave(binary_to_list(Node));
leave(Node) when is_list(Node) ->
    leave(ekka_node:parse_name(Node));
leave(Node) when is_atom(Node) ->
    case rpc:call(Node, ekka, leave, []) of
        {badrpc, Reason} ->
            {error, Reason};
        Res ->
            Res
    end.

get_nodes() ->
    [format(Node, Info) || {Node, Info} <- emqx_mgmt:list_nodes()].


get_nodes(Sort) ->
    RowFun =
        fun({{Node, node}, Info}) ->
            Info#{node => Node}
        end,
    case dgiot_mnesia:match_object({{'$1', node}, '$2'}, RowFun) of
        [] -> [];
        Nodes -> lists:sort(Sort, Nodes)
    end.

choose_node() ->
    Desc =
        fun(#{total_memory := TotalMem1, used_memory := UserMem1, load15 := Load151},
                #{total_memory := TotalMem2, used_memory := UserMem2, load15 := Load152}) ->
            (TotalMem1 - UserMem1) / (1024 * 1024 * 1024 * Load151) >
                (TotalMem2 - UserMem2) / (1024 * 1024 * 1024 * Load152)
        end,
    case get_nodes(fun(Node1, Node2) -> Desc(Node1, Node2) end) of
        [] ->
            {error, node_empty};
        [#{node := Node} | _] ->
            {ok, Node}
    end.


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    State = #state{  },
    register_service(State),
    dgiot_mqtt:subscribe(?GLOBAL_TOPIC),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({deliver, Topic, Message}, State) ->
    dgiot_hook:run_hook(Topic, [Message]),
    {noreply, State};

handle_info(register_service, State) ->
%%    register_service(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dgiot_mnesia:delete({node(), node}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register_service(#state{  }) ->
    try
        Info = get_info(),
        Now = dgiot_datetime:nowstamp(),
        maps:fold(
            fun(Key, Value, _) ->
                dgiot_metrics:gauge(dgiot_global, <<"node">>, [Key], Value)
            end, no, Info),
        dgiot_mnesia:insert({node(), node}, Info#{pid => self(), update => Now})
    catch
        Err:Reason ->
            ?LOG(error,"~p:~p", [Err, Reason])
    after
        erlang:send_after(60000, self(), register_service)
    end.


get_info() ->
    Memory = get_memory(),
    Map = get_cpu(),
    Map#{
        allocated_memory => proplists:get_value(allocated, Memory),
        total_memory => proplists:get_value(total_memory, Memory),
        used_memory => proplists:get_value(used, Memory),
        max_fds => proplists:get_value(max_fds, lists:usort(lists:flatten(erlang:system_info(check_io))))
    }.

get_cpu() ->
    maps:from_list([{K, list_to_float(V)} || {K, V} <- cpu_loads()]).

get_ports() ->
    Ports = string:tokens(dgiot:get_env(monitor_ports, ""), ","),
    lists:foldl(
        fun(Port, Acc) ->
            Key = list_to_binary(lists:concat([tcp, Port])),
            Acc#{
                Key => dgiot_transport:get_port(Port)
            }
        end, #{}, Ports).


get_memory() ->
    Data = memsup:get_system_memory_data(),
    [{Key, get_memory(Key, current)} || Key <- [used, allocated, unused, usage]] ++ erlang:memory() ++
        [{total_memory, proplists:get_value(total_memory, Data)}].

get_memory(used, Keyword) ->
    lists:sum(lists:map(fun({_, Prop}) ->
        container_size(Prop, Keyword, blocks_size)
                        end, util_alloc()));
get_memory(allocated, Keyword) ->
    lists:sum(lists:map(fun({_, Prop}) ->
        container_size(Prop, Keyword, carriers_size)
                        end, util_alloc()));
get_memory(unused, Keyword) ->
    get_memory(allocated, Keyword) - get_memory(used, Keyword);
get_memory(usage, Keyword) ->
    get_memory(used, Keyword) / get_memory(allocated, Keyword).

container_size(Prop, Keyword, Container) ->
    Sbcs = container_value(Prop, Keyword, sbcs, Container),
    Mbcs = container_value(Prop, Keyword, mbcs, Container),
    Sbcs + Mbcs.

container_value(Prop, Keyword, Type, Container) when is_atom(Keyword) ->
    container_value(Prop, 2, Type, Container);
container_value(Props, Pos, mbcs = Type, Container) when is_integer(Pos) ->
    Pool = case proplists:get_value(mbcs_pool, Props) of
               PoolProps when PoolProps =/= undefined ->
                   element(Pos, lists:keyfind(Container, 1, PoolProps));
               _ ->
                   0
           end,
    TypeProps = proplists:get_value(Type, Props),
    Pool + element(Pos, lists:keyfind(Container, 1, TypeProps));

container_value(Props, Pos, Type, Container) ->
    TypeProps = proplists:get_value(Type, Props),
    element(Pos, lists:keyfind(Container, 1, TypeProps)).


util_alloc() ->
    alloc(?UTIL_ALLOCATORS).

alloc() ->
    {_Mem, Allocs} = snapshot_int(),
    Allocs.
alloc(Type) ->
    [{{T, Instance}, Props} || {{T, Instance}, Props} <- alloc(), lists:member(T, Type)].

snapshot_int() ->
    {erlang:memory(), allocators()}.

allocators() ->
    UtilAllocators = erlang:system_info(alloc_util_allocators),
    Allocators = [sys_alloc, mseg_alloc | UtilAllocators],
    [{{A, N}, lists:sort(proplists:delete(versions, Props))} ||
        A <- Allocators, Allocs <- [erlang:system_info({allocator, A})],
        Allocs =/= false, {_, N, Props} <- Allocs].


cpu_loads() ->
    case os:type() of
        {win32, _} ->
            [];
        _ ->
            [{load1, ftos(cpu_sup:avg1() / 256)}, {load5, ftos(cpu_sup:avg5() / 256)}, {load15, ftos(cpu_sup:avg15() / 256)}]
    end.

ftos(F) ->
    io_lib:format("~.2f", [F]).


format(Node, {error, Reason}) -> [{node, Node}, {error, Reason}];

format(Node, Info = #{memory_total := Total, memory_used := Used}) ->
    Info#{node => Node,
        is_current => Node == node(),
        memory_total => emqx_mgmt_util:kmg(Total),
        memory_used  => emqx_mgmt_util:kmg(Used)
    }.



%%ekka_callback(prepare) ->
%%    ekka:callback(prepare, fun ?MODULE:shutdown/1);
%%ekka_callback(reboot) ->
%%    ekka:callback(reboot, fun ?MODULE:reboot/0).

% emqx:shutdown(Action).
shutdown(_Action) ->
    %emqx_alarm_handler:unload(),
    %lists:foreach(fun application:stop/1, [emqx, ekka, cowboy, ranch, esockd, gproc]).
    ok.

reboot() ->
    %emqx:reboot().
    ok.
