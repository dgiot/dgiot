%%
%% Erlang Redis Sentinel connection module
%%
%% Redis Sentinel is a standart way to failover master in the cluster of redis nodes.
%% It is separate process that monitors cluster master and slaves. Normally there are multiple sentinels looking for cluster.
%% After master shutdown they collaborate to ellect new master from slave nodes.
%%
%% More information here:
%%  Sentinel documentation - http://redis.io/topics/sentinel
%%  Recomendations for clients authors - http://redis.io/topics/sentinel-clients
%%
%% Every sentinel can monitor multiple redis clusters. Every cluster has it name (master name).
%% At one time only one redis node in cluster can be master, the others are slaves.
%% This module starts process that keep track of all clusters and sentinels that whatch for them.
%% When client wants to connect to cluster it asks sentinels of that cluster which node is master and returns this
%% information to client.
%%
%% Usage:
%%    {ok, SentinelConn} = eredis_sentinel:start_link(["sentinel-1.lan", {"sentinel-2.lan",26340}]),
%%    {ok, Host, Port}   = eredis_sentinel:get_master(session).
%% When client needs notifications about master changes:
%%    {ok, Host, Port}   = eredis_sentinel:get_master(session, [notify]}.
%% When sentinel process will see that master changed it will send notifications to all subscribers for that master:
%%    {sentinel, {master_reconnect, Host, Port}}
%%
%% TBD: it is possible to have different sets of sentinels for different clusters. Do we need support for this case?
%% TODO: now eredis_sentinel knows that master changes only after get_master request. It is possible to receive notifications
%%       about this through redis pub/sub channel.

-module(eredis_sentinel).
-author("Mikl Kurkov <mkurkov@gmail.com>").

-include("eredis.hrl").
-include("eredis_sentinel.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2, stop/0]).
-export([get_master/1, get_master/2, get_current_sentinel/0]).

%% GenServer
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(sentinel, {
          host :: string(),
          port :: integer()
         }).

-record(errors, {
          sentinel_unreachable = 0 :: integer(),
          master_unknown = 0       :: integer(),
          master_unreachable = 0   :: integer(),
          total = 0                :: integer()
         }).

-record(state, {
          sentinels :: [#sentinel{}],
          conn_pid  :: undefined | pid(),
          masters   :: eredis_sentinel_master:masters(),
          errors    :: #errors{}
         }).

-record(get_master_req, {
          master :: master_name(),
          notify :: boolean()
         }).

%%% API ---------------------------------------------------------------
start_link() ->
    start_link(["localhost"]).

start_link(Sentinels) ->
    start_link(Sentinels, []).

start_link(Sentinels, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Sentinels, Opts], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_master(MasterName) ->
    get_master(MasterName, false).

get_master(MasterName, Notify) when is_atom(MasterName), is_boolean(Notify) ->
    gen_server:call(?MODULE, #get_master_req{master=MasterName, notify=Notify}).

get_current_sentinel() ->
    gen_server:call(?MODULE, get_current_sentinel).

%%% GenServer ---------------------------------------------------------

init([Sentinels,  Opts]) ->
    process_flag(trap_exit, true),
    erlang:put(redis_opts, Opts),
    State = #state{
      sentinels = [read_sentinel(S) || S <- Sentinels],
      conn_pid  = undefined,
      masters   = [],
      errors    = #errors{} },
    {ok, State}.

handle_call(#get_master_req{master=MasterName, notify=Notify}, {FromPid,_Tag}, State) ->
    case query_master(MasterName, State#state{errors = #errors{}}) of
        {ok, {Host,Port}, S1} ->
            {ok,Masters1} = eredis_sentinel_masters:update(S1#state.masters, MasterName, Host, Port),
            {ok,Masters2} =
                case Notify of
                    true ->
                        eredis_sentinel_masters:subscribe(Masters1, MasterName, FromPid);
                    false ->
                        {ok, Masters1}
                end,
            {reply, {ok, {Host,Port}}, S1#state{masters=Masters2}};
        {error, Error, S1} ->
            {reply, {error, Error}, S1}
    end;

handle_call(get_current_sentinel, _From, #state{sentinels=[S|_],conn_pid=ConnPid} = State) ->
    Res = {S#sentinel.host, S#sentinel.port, ConnPid},
    {reply, {ok, Res}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Current sentinel connection broken
handle_info({'EXIT', Pid, _Reason}, #state{conn_pid = Pid} = S) ->
    {noreply, S#state{conn_pid = undefined}};

%% Ignore late exit messages
handle_info({'EXIT', _Pid, _Reason}, S) ->
    {noreply, S};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal ----------------------------------------------------------

read_sentinel(Host) when is_list(Host) ->
    #sentinel{host=Host, port=?SENTINEL_PORT};
read_sentinel({Host,Port}) when is_list(Host), is_integer(Port) ->
    #sentinel{host=Host, port=Port}.


%% Finding new master host for named cluster:
%% * First try to query already connected sentinel if we have one.
%% * If this failed try to connect and query all sentinels starting from the last connected one.
%% * If connected sentinel returns port:ip - return {ok, {Host, Port}} and remember connection pid.
%% * In case no sentinels return valid answer - response with error:
%%   * If all sentinels failed connect to - return {error, sentinel_unreachable}
%%   * If all connected sentinels return null - return {error, sentinel_master_unknown}
%%   * If some of connected sentinels return -IDONTKNOW - return {error, sentinel_master_unreachable}

-spec query_master(master_name(), #state{}) ->
    {ok, {master_host(), master_port()}, #state{}} | {error, any(), #state{}}.

%% All sentinels return errors
query_master(_MasterName, #state{errors=Errors,sentinels=Sentinels} = S)
  when Errors#errors.total >= length(Sentinels) ->
    #errors{sentinel_unreachable=SU, master_unknown=MUK, master_unreachable=MUR} = Errors,
    if
        SU == length(Sentinels) ->
            {error, ?SENTINEL_UNREACHABLE,S};
        MUK > 0, MUR == 0 ->
            {error, ?MASTER_UNKNOWN,S};
        true ->
            {error, ?MASTER_UNREACHABLE,S}
    end;

%% No connected sentinel
query_master(MasterName, #state{conn_pid=undefined, sentinels = [#sentinel{host=H,port=P} | _] } = S) ->
    Opts = case erlang:get(redis_opts) of
        undefined -> [];
        Opts0 -> Opts0
    end,
    case eredis_sentinel_client:start_link(H,P, Opts) of
        {ok, ConnPid} ->
            query_master(MasterName, S#state{conn_pid=ConnPid});
        {error, E} ->
            error_logger:error_msg("Error connecting to sentinel at ~p:~p : ~p~n",[H,P,E]),
            Errors = update_errors(?SENTINEL_UNREACHABLE, S#state.errors),
            Sentinels = rotate(S#state.sentinels),
            query_master(MasterName, S#state{sentinels=Sentinels, errors=Errors})
    end;

%% Sentinel connected
query_master(MasterName, #state{conn_pid=ConnPid, sentinels=[#sentinel{host=H,port=P}|_]} = S) when is_pid(ConnPid)->
    case eredis_sentinel_client:get_master(ConnPid, MasterName) of
        {ok, HostPort} ->
            {ok, HostPort, S};
        {error, Error} ->
            error_logger:error_msg("Master request for ~p to sentinel ~p:~p failed with ~p~n", [MasterName,H,P,Error]),
            eredis_sentinel_client:stop(ConnPid),
            Errors = update_errors(Error, S#state.errors),
            Sentinels = rotate(S#state.sentinels),
            query_master(MasterName, S#state{conn_pid = undefined, errors=Errors, sentinels=Sentinels})
    end.

update_errors(E, #errors{sentinel_unreachable=SU, master_unknown=MUK, master_unreachable=MUR, total = T} = Errors) ->
    Errors1 =
        case E of
            ?SENTINEL_UNREACHABLE ->
                Errors#errors{sentinel_unreachable = SU + 1};
            ?MASTER_UNKNOWN ->
                Errors#errors{master_unknown = MUK + 1};
            ?MASTER_UNREACHABLE ->
                Errors#errors{master_unreachable = MUR + 1}
        end,
    Errors1#errors{total = T + 1}.


rotate([])     ->    [];
rotate([X|Xs]) -> Xs ++ [X].


%%% Unit tests --------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rotate_test() ->
    ?assertEqual([],  rotate([])),
    ?assertEqual([1], rotate([1])),
    ?assertEqual([2,3,1], rotate([1,2,3])).

-endif.
