-module(eredis_cluster_monitor).
-behaviour(gen_server).

%% API.
-export([start_link/2]).
-export([refresh_mapping/2]).
-export([get_state/1, get_state_version/1]).
-export([get_pool_by_slot/2]).
-export([get_all_pools/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Type definition.
-include("eredis_cluster.hrl").
-record(state, {
    init_nodes :: [#node{}],
    slots :: tuple(), %% whose elements are integer indexes into slots_maps
    slots_maps :: tuple(), %% whose elements are #slots_map{}
    version :: integer(),
    pool_name :: atom(),
    database = 0 :: integer(),
    password = "" :: string(),
    size     = 10 :: integer(),
    max_overflow = 0 :: integer()
}).

%% API.
start_link(Name, Opts) ->
    gen_server:start_link({local, name(Name)}, ?MODULE, [Name, Opts], []).

refresh_mapping(Name, Version) ->
    case whereis(name(Name)) of
        undefined -> {error, not_find_process};
        Pid -> gen_server:call(Pid, {reload_slots_map, Version})
    end.

%% =============================================================================
%% @doc Given a slot return the link (Redis instance) to the mapped
%% node.
%% @end
%% =============================================================================
get_state(Name) ->
    case ets:lookup(?MODULE, Name) of
        undefined -> #state{};
        [{Name, State}] -> State
    end.

get_state_version(State) ->
    State#state.version.

get_all_pools(Name) ->
    State = get_state(Name),
    SlotsMapList = tuple_to_list(State#state.slots_maps),
    [SlotsMap#slots_map.node#node.pool || SlotsMap <- SlotsMapList,
        SlotsMap#slots_map.node =/= undefined].

%% =============================================================================
%% @doc Get cluster pool by slot. Optionally, a memoized State can be provided
%% to prevent from querying ets inside loops.
%% @end
%% =============================================================================
get_pool_by_slot(Slot, State) when is_integer(Slot) ->
    Index = element(Slot+1,State#state.slots),
    Cluster = element(Index,State#state.slots_maps),
    if
        Cluster#slots_map.node =/= undefined ->
            {Cluster#slots_map.node#node.pool, State#state.version};
        true ->
            {undefined, State#state.version}
    end;

get_pool_by_slot(Name, Slot) ->
    State = get_state(Name),
    get_pool_by_slot(Slot, State).

-spec reload_slots_map(State::#state{}) -> NewState::#state{}.
reload_slots_map(State = #state{pool_name = PoolName}) ->
    NewState = case get_cluster_slots(State#state.init_nodes, State, 0) of
        {error, _Reason} ->
            State#state{init_nodes = []};
        [] -> State#state{version = State#state.version + 1};
        ClusterSlots ->
            [close_connection(SlotsMap)
                || SlotsMap <- tuple_to_list(State#state.slots_maps)],
            SlotsMaps = parse_cluster_slots(ClusterSlots),
            ConnectedSlotsMaps = connect_all_slots(SlotsMaps, State),
            Slots = create_slots_cache(ConnectedSlotsMaps),
            State#state{
                slots = list_to_tuple(Slots),
                slots_maps = list_to_tuple(ConnectedSlotsMaps),
                version = State#state.version + 1
            }
    end,
    true = ets:insert(?MODULE, [{PoolName, NewState}]),
    NewState.

get_cluster_slots([], State, FailAcc) ->
    case erlang:length(State#state.init_nodes) =:= FailAcc of
        true ->
            {error, <<"ERR all nodes are down">>};
        false ->
            []
    end;

get_cluster_slots([Node|T], State, FailAcc) ->
    case safe_eredis_start_link(Node, State) of
        {ok,Connection} ->
          case eredis:q(Connection, ["CLUSTER", "SLOTS"]) of
            {error,<<"ERR unknown command 'CLUSTER'">>} ->
                get_cluster_slots_from_single_node(Node);
            {error,<<"ERR This instance has cluster support disabled">>} ->
                get_cluster_slots_from_single_node(Node);
            {ok, ClusterInfo} ->
                eredis:stop(Connection),
                ClusterInfo;
            _ ->
                eredis:stop(Connection),
                get_cluster_slots(T, State, FailAcc+1)
        end;
        _ ->
            get_cluster_slots(T, State, FailAcc+1)
  end.

-spec get_cluster_slots_from_single_node(#node{}) ->
    [[bitstring() | [bitstring()]]].
get_cluster_slots_from_single_node(Node) ->
    [[<<"0">>, integer_to_binary(?REDIS_CLUSTER_HASH_SLOTS-1),
    [list_to_binary(Node#node.address), integer_to_binary(Node#node.port)]]].

-spec parse_cluster_slots([[bitstring() | [bitstring()]]]) -> [#slots_map{}].
parse_cluster_slots(ClusterInfo) ->
    parse_cluster_slots(ClusterInfo, 1, []).

parse_cluster_slots([[StartSlot, EndSlot | [[Address, Port | _] | _]] | T], Index, Acc) ->
    SlotsMap =
        #slots_map{
            index = Index,
            start_slot = binary_to_integer(StartSlot),
            end_slot = binary_to_integer(EndSlot),
            node = #node{
                address = binary_to_list(Address),
                port = binary_to_integer(Port)
            }
        },
    parse_cluster_slots(T, Index+1, [SlotsMap | Acc]);
parse_cluster_slots([], _Index, Acc) ->
    lists:reverse(Acc).



-spec close_connection(#slots_map{}) -> ok.
close_connection(SlotsMap) ->
    Node = SlotsMap#slots_map.node,
    if
        Node =/= undefined ->
            try eredis_cluster_pool:stop(Node#node.pool) of
                _ ->
                    ok
            catch
                _ ->
                    ok
            end;
        true ->
            ok
    end.

connect_node(Node = #node{address  = Host, port = Port}, #state{database = DataBase,
                                                                password = Password,
                                                                size     = Size,
                                                                max_overflow = MaxOverflow}) ->
    Options = case erlang:get(options) of
        undefined -> [];
        Options0 -> Options0
    end,
    case eredis_cluster_pool:create(Host, Port, DataBase, Password, Size, MaxOverflow, Options) of
        {ok, Pool} ->
            Node#node{pool = Pool};
        _ ->
            undefined
    end.

safe_eredis_start_link(#node{address = Host, port = Port},
                       #state{database = DataBase, password = Password}) ->
    Options = case erlang:get(options) of
        undefined -> [];
        Options0 -> Options0
    end,
    eredis:start_link(Host, Port, DataBase, Password, no_reconnect, 5000, Options).

-spec create_slots_cache([#slots_map{}]) -> [integer()].
create_slots_cache(SlotsMaps) ->
  SlotsCache = [[{Index,SlotsMap#slots_map.index}
        || Index <- lists:seq(SlotsMap#slots_map.start_slot,
            SlotsMap#slots_map.end_slot)]
        || SlotsMap <- SlotsMaps],
  SlotsCacheF = lists:flatten(SlotsCache),
  SortedSlotsCache = lists:sort(SlotsCacheF),
  [ Index || {_,Index} <- SortedSlotsCache].

connect_all_slots(SlotsMapList, State) ->
    [SlotsMap#slots_map{node=connect_node(SlotsMap#slots_map.node, State)}
        || SlotsMap <- SlotsMapList].

connect_(PoolName, Opts) ->
    erlang:put(options, proplists:get_value(options, Opts, [])),
    State = #state{
        slots = undefined,
        slots_maps = {},
        init_nodes = [#node{address= A, port = P} || {A,P} <- proplists:get_value(servers, Opts, [])],
        version = 0,
        pool_name = PoolName,
        database = proplists:get_value(database, Opts, 0),
        password = proplists:get_value(password, Opts, ""),
        size     = proplists:get_value(pool_size, Opts, 10),
        max_overflow = proplists:get_value(pool_max_overflow, Opts, 0)
    },
    reload_slots_map(State).

%% gen_server.

init([PoolName, Opts]) ->
    process_flag(trap_exit, true),
    case connect_(PoolName, Opts) of
        #state{init_nodes = []} ->
            {stop, <<"ERR unable to connect to any nodes">>};
        State ->
            {ok, State}
    end.

handle_call({reload_slots_map,Version}, _From, #state{version=Version} = State) ->
    {reply, ok, reload_slots_map(State)};
handle_call({reload_slots_map,_}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{slots_maps = Slots}) ->
    SlotsMapList = tuple_to_list(Slots),
    [eredis_cluster_pool:stop(SlotsMap#slots_map.node#node.pool) ||
        SlotsMap <- SlotsMapList, SlotsMap#slots_map.node =/= undefined],
    ok.
% Down
code_change(_, State, _Extra) ->
    {ok, State}.

name(Name) ->
    list_to_atom("monitor_" ++ atom_to_list(Name)).
