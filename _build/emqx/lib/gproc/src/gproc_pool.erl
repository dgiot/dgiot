%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% --------------------------------------------------
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% --------------------------------------------------
%%
%% @author Ulf Wiger <ulf@wiger.net>
%%
%% @doc Load balancing functions based on Gproc.
%%
%% This module implements support for load-balancing server pools. It was
%% originally intended mainly as an example of how to use various Gproc
%% resources (e.g. counters and shared properties), but is fully integrated
%% into Gproc, and fully functional.
%%
%% <h2>Concepts</h2>
%%
%% Each pool has a list of 'named' workers (defined using `add_worker/2') and
%% a load-balancing strategy. Processes can then 'connect' to the pool (with
%% `connect_worker/2'), using one of the defined names.
%%
%% Users then 'pick' one of the currently connected processes in the pool. Which
%% process is picked depends on the load-balancing strategy.
%%
%% The whole representation of the pool and its connected workers is in gproc.
%% The server `gproc_pool' is used to serialize pool management updates, but
%% worker selection is performed entirely in the calling process, and can be
%% performed by several processes concurrently.
%%
%% <h3>Load-balancing strategies</h3>
%%
%% * `round_robin' is the default. A wrapping gproc counter keeps track of the
%%   latest worker picked, and `gproc:next()' is used to find the next worker.
%% * `random' picks a random worker from the pool.
%% * `hash' requires a value (`pick/2'), and picks a worker based on the hash of
%%   that value.
%% * `direct' takes an integer as an argument, and picks the next worker (modulo
%%   the size of the pool). This is mainly for implementations that implement
%%   a load-balancing strategy on top of `gproc_pool'.
%% * `claim' picks the first available worker and 'claims' it while executing
%%   a user-provided fun. This means that the number of concurrently executing
%%   jobs will not exceed the size of the pool.
%% @end
-module(gproc_pool).
-behavior(gen_server).

%% gproc round-robin name lookup
-export([new/1,                % (Pool) -> (Pool, round_robin, [])
         new/3,                % (Pool, Type, Opts)
         delete/1,             % (Pool)
         force_delete/1,       % (Pool)
         add_worker/2,         % (Pool, Name)      -> Pos
         add_worker/3,         % (Pool, Name, Pos) -> Pos
         remove_worker/2,      % (Pool, Name)
         connect_worker/2,     % (Pool, Name)
         disconnect_worker/2,  % (Pool, Name)
         whereis_worker/2,     % (Pool, Name)
         worker_id/2,          % (Pool, Name)
         active_workers/1,     % (Pool)
         defined_workers/1,    % (Pool)
         worker_pool/1,        % (Pool)
         pick/1,               % (Pool)
         pick/2,               % (Pool, Value)
         pick_worker/1,        % (Pool)
         pick_worker/2,        % (Pool, Value)
         claim/2,              % (Pool, Fun)
	 claim/3,              % (Pool, Fun, Wait)
         log/1,                % (WorkerId)
         randomize/1]).        % (Pool)

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([test/1, test/3, ptest/4, test_run/2, test_run1/2, test_run2/2,
         test_run0/2, setup_test_pool/3, setup_test_pool/4,
	 remove_test_pool/1]).

-define(POOL(Pool), {p,l,{?MODULE,Pool}}).
-define(POOL_CUR(Pool), {c,l,{?MODULE,Pool,cur}}).
-define(POOL_WRK(Pool,Name), {c,l,{?MODULE,Pool,w,Name}}).

-record(st, {}).

%% @spec new(Pool::any()) -> ok
%%
%% @equiv new(Pool, round_robin, [])
new(Pool) ->
    new(Pool, round_robin, []).

%% @spec new(Pool::any(), Type, Opts) -> true
%%     Type = round_robin | random | hash | direct | claim
%%     Opts = [{size, integer()} | {auto_size, boolean()}]
%%
%% @doc Create a new pool.
%%
%% The pool starts out empty. If a size is not given, the pool size is set to
%% 0 initially. `auto_size' is `true' by default if size is not specified, but
%% `false' by default otherwise. If `auto_size == true', the pool will be
%% enlarged to accomodate new workers, when necessary. Otherwise, trying to add
%% a worker when the pool is full will raise an exception, as will trying to add
%% a worker on a specific position beyond the current size of the pool.
%%
%% If the given pool already exists, this function will raise an exception.
%% @end
new(Pool, Type, Opts) when Type == round_robin;
                           Type == random;
                           Type == hash;
                           Type == direct;
                           Type == claim ->
    call({new, Pool, Type, Opts}).

%% @spec delete(Pool::any()) -> true
%% @doc Delete an existing pool.
%%
%% This function will delete a pool, only if there are no connected workers.
%% Ensure that workers have been disconnected before deleting the pool.
%% @end
%%
delete(Pool) ->
    call({delete, Pool}).

%% @spec force_delete(Pool::any()) -> true
%% @doc Forcibly remove a pool, terminating all active workers
%%
%% This function is primarily intended for cleanup of any pools that might have
%% become inconsistent (for whatever reason). It will clear out all resources
%% belonging to the pool and send `exit(Pid, kill)' signals to all connected
%% workers (except the calling process).
%% @end
%%
force_delete(Pool) ->
    %% This is not pretty, but this function is mainly intended to clean up
    %% a pool that's not used, with no regard to connected workers, except self(),
    %% (that is, we kill each connected worker). We don't worry about races,
    %% so don't go to the server (which doesn't have own state on the pool
    %% anyway).
    force_delete_(Pool).

%% @spec add_worker(Pool::any(), Name::any()) -> integer()
%%
%% @doc Assign a worker name to the pool, returning the worker's position.
%%
%% Before a worker can connect to the pool, its name must be added. If no explicit
%% position is given (see {@link add_worker/3}), the most suitable position,
%% depending on load-balancing algorithm, is selected: for round_robin and direct
%% pools, names are packed tightly from the beginning; for hash and random pools,
%% slots are filled as sparsely as possible, in order to maintain an even
%% likelihood of hitting each worker.
%%
%% An exception is raised if the pool is full (and `auto_size' is false), or if
%% `Name' already exists in the pool.
%%
%% Before a worker can be used, a process must connect to it (see
%% {@link connect_worker/2}.
%% @end
add_worker(Pool, Name) ->
    call({add_worker, Pool, Name}).

%% @spec add_worker(Pool::any(), Name::any(), Slot::integer()) -> integer()
%%
%% @doc Assign a worker name to a given slot in the pool, returning the slot.
%%
%% This function allows the pool maintainer to exactly position each worker
%% inside the pool. An exception is raised if the position is already taken,
%% or if `Name' already exists in the pool. If `Slot' is larger than the current
%% size of the pool, an exception is raised iff `auto_size' is `false';
%% otherwise the pool is expanded to accomodate the new position.
%% @end
add_worker(Pool, Name, Slot) ->
    call({add_worker, Pool, Name, Slot}).

%% @spec connect_worker(Pool::any(), Name::any()) -> true
%% @doc Connect the current process to `Name' in `Pool'.
%%
%% Typically, a server will call this function as it starts, similarly to when
%% it registers itself. In fact, calling `connect_worker/2' leads to the process
%% being registered as `{n,l,[gproc_pool,N,Name]}', where `N' is the position of
%% `Name' in the pool. This means (a) that gproc monitors the worker, and
%% removes the connection automatically if it dies, and (b) that the registered
%% names can be listed in order of their positions in the pool.
%%
%% This function raises an exception if `Name' does not exist in `Pool' (or
%% there is no such pool), or if another worker is already connected to
%% `Name'.
%% @end
%%
connect_worker(Pool, Name) ->
    gproc:reg(worker_id(Pool, Name), 0).

%% @spec disconnect_worker(Pool, Name) -> true
%%
%% @doc Disconnect the current process from `Name' in `Pool'.
%%
%% This function is similar to a `gproc:unreg()' call. It removes the
%% connection between `Pool', `Name' and pid, and makes it possible for another
%% process to connect to `Name'.
%%
%% An exception is raised if there is no prior connection between `Pool',
%% `Name' and the current process.
%% @end
%%
disconnect_worker(Pool, Name) ->
    gproc:unreg(worker_id(Pool, Name)).

%% @spec remove_worker(Pool::any(), Name::any()) -> true
%% @doc Remove a previously added worker.
%%
%% This function will assume that any connected worker is disconnected first.
%% It will fail if there is no such pool, but will return `true' in the case
%% when `Name' did not exist in the pool in the first place.
%% @end
remove_worker(Pool, Name) ->
    call({remove_worker, Pool, Name}).

%% @spec whereis_worker(Pool::any(), Name::any()) -> pid() | undefined
%% @doc Look up the pid of a connected worker.
%%
%% This function works similarly to `gproc:where/1': it will return the pid
%% of the worker connected as `Pool / Name', if there is such a worker; otherwise
%% it will return `undefined'. It will raise an exception if `Name' has not been
%% added to the pool.
%% @end
whereis_worker(Pool, Name) ->
    ID = worker_id(Pool, Name),
    gproc:where(ID).

%% @spec worker_id(Pool, Name) -> GprocName
%% @doc Return the unique gproc name corresponding to a name in the pool.
%%
%% This function assumes that `Name' has been added to `Pool'. It returns the
%% unique name that a connected worker will be registered as. This doesn't mean
%% that there is, in fact, such a connected worker.
%% @end
worker_id(Pool, Name) ->
    N = gproc:get_attribute(?POOL_WRK(Pool, Name), shared, n),
    {n, l, [?MODULE, Pool, N, Name]}.

%% @spec active_workers(Pool::any()) -> [{Name, Pid}]
%% @doc Return a list of currently connected workers in the pool.
%%
active_workers(Pool) ->
    gproc:select(
      {l,n},
      [{ {{n,l,[?MODULE,Pool,'$1','$2']},'$3','_'}, [{is_integer, '$1'}],
         [{{'$2', '$3'}}] }]).

%% @spec defined_workers(Pool::any()) -> [{Name, Pos, Count}]
%% @doc Return a list of added workers in the pool.
%%
%% The added workers are slots in the pool that have been given names, and thus
%% can be connected to. This function doesn't detect whether or not there are
%% any connected (active) workers.
%%
%% The list contains `{Name, Pos, Count}', where `Name' is the name of the added
%% worker, `Pos' is its position in the pool, and `Count' represents the number
%% of times the worker has been picked (assuming callers keep count by explicitly
%% calling {@link log/1}).
%% @end
defined_workers(Pool) ->
    K = ?POOL(Pool),
    [{N, Pos, gproc:get_value(?POOL_WRK(Pool, N), shared)}
     || {N, Pos} <- get_workers_(K)].

%% @spec worker_pool(Pool::any()) -> [integer() | {Name, Pos}]
%% @doc Return a list of slots and/or named workers in the pool.
%%
%% This function is mainly for testing, but can also be useful when implementing
%% your own worker placement algorithm on top of gproc_pool.
%%
%% A plain integer represents an unfilled slot, and `{Name, Pos}' represents an
%% added worker. The pool is always filled to the current size.
%% @end
worker_pool(Pool) ->
    get_workers_(?POOL(Pool)).

%% @spec pick(Pool::any()) -> GprocName | false
%% @doc Pick a worker from the pool given the pool's load-balancing algorithm.
%%
%% The pool types that allows picking without an extra argument are
%% round_robin and random. This function returns `false' if there is no available
%% worker, or if `Pool' is not a valid pool.
%% @end
pick(Pool) ->
    case gproc:get_value(?POOL(Pool), shared) of
        {0, _} -> false;
        {Sz, Type} when Type == round_robin; Type == random ->
            pick(Pool, Sz, Type, name);
        _ ->
            error(badarg)
    end.

%% @spec pick_worker(Pool::any()) -> pid() | false
%% @doc Pick a worker pid from the pool given the pool's load-balancing algorithm.
%%
%% Like {@link pick/1}, but returns the worker pid instead of the name.
%% @end
pick_worker(Pool) ->
    case gproc:get_value(?POOL(Pool), shared) of
        {0, _} -> false;
        {Sz, Type} when Type == round_robin; Type == random ->
            pick(Pool, Sz, Type, pid);
        _ ->
            error(badarg)
    end.

%% @spec pick(Pool::any(), Value::any()) -> GprocName | false
%% @doc Pick a worker from the pool based on `Value'.
%%
%% The pool types that allows picking based on an extra argument are
%% hash and direct. This function returns `false' if there is no available
%% worker, or if `Pool' is not a valid pool.
%%
%% If the pool is of type `direct', `Value' must be an integer corresponding to
%% a position in the pool (modulo the size of the pool). If the type is
%% `hash', `Value' may be any term, and its hash value will serve as a guide for
%% selecting a worker.
%% @end
pick(Pool, N) ->
    case gproc:get_value(?POOL(Pool), shared) of
        {0, _} -> false;
        {Sz, Type} when Type == hash; Type == direct ->
            pick(Pool, Sz, Type, N, name);
        _ ->
            error(badarg)
    end.

%% @spec pick_worker(Pool::any(), Value::any()) -> pid() | false
%% @doc Pick a worker pid from the pool given the pool's load-balancing algorithm.
%%
%% Like {@link pick/2}, but returns the worker pid instead of the name.
%% @end
pick_worker(Pool, N) ->
    case gproc:get_value(?POOL(Pool), shared) of
        {0, _} -> false;
        {Sz, Type} when Type == hash; Type == direct ->
            pick(Pool, Sz, Type, N, pid);
        _ ->
            error(badarg)
    end.

pick(Pool, Sz, round_robin, Ret) ->
    Next = incr(Pool, 1, Sz),
    case ets:next(gproc, {{n,l,[?MODULE,Pool,Next]},n}) of
        {{n,l,[?MODULE,Pool,Actual,_Name]} = Pick, _} ->
            case Actual - Next of
                Diff when Diff > 1 ->
                    gproc:update_counter(
                      ?POOL_CUR(Pool), shared, {Diff, Sz, 1}),
                    ret(Pick, Ret);
                _ ->
                    ret(Pick, Ret)
            end;
        _ ->
            case ets:next(gproc, {{n,l,[?MODULE,Pool,0]}, n}) of
                {{n,l,[?MODULE,Pool,Actual1,_Name1]} = Pick, _} ->
                    incr(Pool, Sz-Next+Actual1, Sz),
                    %% gproc:update_counter(
                    %%   ?POOL_CUR(Pool), shared, {Sz-Next+Actual1, Sz, 1}),
                    ret(Pick, Ret);
                _ ->
                    false
            end
    end;
pick(Pool, Sz, random, Ret) ->
    pick_near(Pool, rand:uniform(Sz + 1), Ret).

pick(Pool, Sz, hash, Val, Ret) ->
    pick_near(Pool, erlang:phash2(Val, Sz) + 1, Ret);
pick(Pool, Sz, direct, N, Ret) when is_integer(N), N > 0 ->
    pick_near(Pool, case (N rem Sz-1) + 1  of 0 -> Sz; N1 -> N1 end, Ret).

pick_near(Pool, N, Ret) ->
    case ets:next(gproc, {{n,l,[?MODULE,Pool,N]}, n}) of
        {{n,l,[?MODULE,Pool,_,_]} = Pick, _} ->
            ret(Pick, Ret);
        _ ->
            %% wrap
            case ets:next(gproc, {{n,l,[?MODULE,Pool,1]}, n}) of
                {{n,l,[?MODULE,Pool,_,_]} = Pick, _} ->
                    ret(Pick, Ret);
                _ ->
                    false
            end
    end.

ret(Name, name) ->
    Name;
ret(Name, pid) ->
    case ets:lookup(gproc, {Name,n}) of
        [{_, Pid, _}] ->
            Pid;
        [] ->
            %% possible race
            false
    end.

%% @equiv claim(Pool, F, nowait)
claim(Pool, F) when is_function(F, 2) ->
    claim(Pool, F, nowait).

%% @spec claim(Pool, Fun, Wait) -> {true, Res} | false
%%         Pool = any()
%%         Fun  = function()
%%         Wait = nowait | {busy_wait, integer()}
%%
%% @doc Picks the first available worker in the pool and applies `Fun'.
%%
%% A `claim' pool allows the caller to "claim" a worker during a short span
%% (essentially, a lock is set and released as soon as `Fun' returns).
%% Once a worker is selected, `Fun(Name, Pid)' is called, where `Name' is a
%% unique gproc name of the worker, and `Pid' is its process identifier.
%% The gproc name of the worker serves as a mutex, where its value is 0 (zero)
%% if the worker is free, and 1 (one) if it is busy. The mutex operation is
%% implemented using `gproc:update_counter/2'.
%%
%% `Wait == nowait' means that the call will return `false' immediately if
%% there is no available worker.
%%
%% `Wait == {busy_wait, Timeout}' will keep repeating the claim attempt
%% for `Timeout' milliseconds. If still no worker is available, it will
%% return `false'.
%% @end
claim(Pool, F, Wait) ->
    case gproc:get_value(?POOL(Pool), shared) of
	{0, _} -> false;
	{_, claim} ->
	    W = setup_wait(Wait, Pool),
	    claim_w(Pool, F, W);
	_ ->
	    error(badarg)
    end.

claim_w(_Pool, _F, timeout) ->
    false;
claim_w(Pool, F, W) ->
    case claim_(Pool, F) of
	false ->
	    claim_w(Pool, F, do_wait(W));
	Other ->
	    clear_wait(W),
	    Other
    end.

%% Define how many workers to select in each chunk. We want to strike
%% a good compromise between the cost of succeeding on the first try
%% (likely a common event) and the cost of retrying. In my measurements,
%% if a chunk size of 1 costs ca 30 us (on my Macbook), a chunk size of 5
%% adds only ca 20% to the cost, i.e. a few us.
-define(CLAIM_CHUNK, 5).

claim_(Pool, F) ->
    %% Sorry, but we use ets:select/3 here in order to shave off a few us.
    case ets:select(gproc, [{ {{{n,l,[?MODULE,Pool,'_','_']},n}, '$1', 0}, [],
			      [{{ {element,1,{element,1,'$_'}}, '$1' }}]}],
		     ?CLAIM_CHUNK) of
        {[_|_] = Workers, Cont} ->
            case try_claim(Workers, F) of
                {true, _} = True ->
                    True;
                false ->
                    claim_cont(Cont, F)
            end;
        _ ->
            false
    end.

claim_cont('$end_of_table', _) ->
    false;
claim_cont(Cont, F) ->
    case ets:select(Cont) of
        {[_|_] = Workers, Cont1} ->
            case try_claim(Workers, F) of
                {true, _} = True ->
                    True;
                false ->
                    claim_cont(Cont1, F)
            end;
        _ ->
            false
    end.

try_claim([], _) ->
    false;
try_claim([{K,Pid}|T], F) ->
    case try_claim(K, Pid, F) of
	false ->
	    try_claim(T, F);
	Other ->
	    Other
    end.

try_claim(K, Pid, F) ->
    case gproc:update_counter(K, [0, {1, 1, 1}]) of
        [0, 1] ->
            %% have lock
            execute_claim(F, K, Pid);
        [1, 1] ->
            %% no
            false
    end.

%% Wrapper to handle the case where the claimant gets killed by another
%% process while executing within the critical section.
%% This is likely a rare case, but if it happens, the claim would never
%% get released.
%% Solution:
%% - spawn a monitoring process which resets the claim if the parent dies
%%   (spawn_link() might be more efficient, but we cannot enable trap_exit
%%    atomically, which introduces a race condition).
%% - for all return types, kill the monitor and release the claim.
%% - the one case where the monitor *isn't* killed is when Parent itself
%%   is killed before executing the `after' clause. In this case, it should
%%   be safe to release the claim from the monitoring process.
%%
%% Overhead in the normal case:
%% - spawning the monitoring process
%% - (possibly scheduling the monitoring process to set up the monitor)
%% - killing the monitoring process (untrappably)
%% Timing the overhead over 100,000 claims on a Core i7 MBP running OTP 17,
%% this wrapper increases the cost of a minimal claim from ca 3 us to
%% ca 7-8 us.
execute_claim(F, K, Pid) ->
    Parent = self(),
    Mon = spawn(
            fun() ->
                    Ref = erlang:monitor(process, Parent),
                    receive
                        {'DOWN', Ref, _, _, _} ->
                            gproc:reset_counter(K)
                    end
            end),
    try begin
            Res = F(K, Pid),
            {true, Res}
        end
    after
        exit(Mon, kill),
        gproc:reset_counter(K)
    end.

setup_wait(nowait, _) ->
    nowait;
setup_wait({busy_wait, MS}, Pool) ->
    Ref = erlang:send_after(MS, self(), {claim, Pool}),
    {busy_wait, Ref}.

do_wait(nowait) ->
    timeout;
do_wait({busy_wait, Ref} = W) ->
    %% Yielding here serves two purposes:
    %% 1) Increase the chance that whoever's before us can finish
    %% 2) The value of read_timer/1 only refreshes after yield (so I've heard)
    erlang:yield(),
    case erlang:read_timer(Ref) of
	false ->
	    erlang:cancel_timer(Ref),
	    timeout;
	_ ->
	    W
    end.

clear_wait(nowait) ->
    ok;
clear_wait({busy_wait, Ref}) ->
    erlang:cancel_timer(Ref),
    ok.

%% @spec log(GprocKey) -> integer()
%% @doc Update a counter associated with a worker name.
%%
%% Each added worker has a gproc counter that can be used e.g. to keep track of
%% the number of times the worker has been picked. Since it's associated with the
%% named 'slot', and not to the connected worker, its value will persist even
%% if the currently connected worker dies.
%% @end
log({n,l,[?MODULE,Pool,_,Name]}) ->
    gproc:update_shared_counter(?POOL_WRK(Pool,Name), 1).

%% @spec randomize(Pool::any()) -> integer()
%% @doc Randomizes the "next" pointer for the pool.
%%
%% This function only has an effect for `round_robin' pools, which have a
%% reference to the next worker to be picked. Without randomizing, the load
%% balancing will always start with the first worker in the pool.
%% @end
randomize(Pool) ->
    case pool_size(Pool) of
        0 -> 0;
        1 -> 1;
        Sz ->
            incr(Pool, rand:uniform(Sz + 1) - 1, Sz)
    end.

%% @spec pool_size(Pool::any()) -> integer()
%% @doc Return the size of the pool.
%%
pool_size(Pool) ->
    {Sz, _} = gproc:get_value(?POOL(Pool), shared),
    Sz.



%% ===================================================================
%% Start, stop, call gen_server

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
    {ok, #st{}}.

%% @private
call(Req) ->
    case gen_server:call(?MODULE, Req) of
        badarg ->
            error(badarg);
        {badarg, Reason} ->
            error(Reason);
        Reply ->
            Reply
    end.

%% ===================================================================
%% Gen_server callbacks

%% @private
handle_call(Req, From, S) ->
    try handle_call_(Req, From, S)
    catch
        error:Reason ->
            {reply, {badarg, Reason}, S}
    end.

handle_call_({new, Pool, Type, Opts}, _, S) ->
    new_(Pool, Type, Opts),
    {reply, ok, S};
handle_call_({delete, Pool}, _, S) ->
    delete_(Pool),
    {reply, ok, S};
handle_call_({force_delete, Pool}, _, S) ->
    force_delete_(Pool),
    {reply, ok, S};
handle_call_({add_worker, Pool, Name}, _, S) ->
    N = add_worker_(Pool, Name),
    {reply, N, S};
handle_call_({add_worker, Pool, Name, Pos}, _, S) ->
    N = add_worker_(Pool, Name, Pos),
    {reply, N, S};
handle_call_({set_pool_size, Pool, Sz}, _, S) ->
    Workers = get_workers_(Pool),
    case get_last_worker_n(Workers) of
        N when N > Sz ->
            {reply, badarg, S};
        _ ->
            set_pool_size_(?POOL(Pool), Sz, Workers),
            {reply, true, S}
    end;
handle_call_({remove_worker, Pool, Name}, _, S) ->
    ok = remove_worker_(Pool, Name),
    {reply, true, S}.

%% @private
handle_cast(_, S) ->
    {noreply, S}.

%% @private
handle_info(_, S) ->
    {noreply, S}.

%% @private
terminate(_, _) ->
    ok.

%% @private
code_change(_, S, _) ->
    {ok, S}.

%% ===================================================================
%% Internal functions


new_(Pool, Type, Opts) ->
    valid_type(Type),
    Size = proplists:get_value(size, Opts, 0),
    Workers = lists:seq(1, Size),
    K = ?POOL(Pool),
    try gproc:reg_shared(K, {Size, Type})
    catch
        error:_ -> error(exists)
    end,
    Opts1 =
        case lists:keyfind(auto_size, 1, Opts) of
            false ->
                Opts ++ [{auto_size, not lists:keymember(size, 1, Opts)}];
            {_, Bool} when is_boolean(Bool) ->
                Opts
        end,
    gproc:set_attributes_shared(K, Opts1),
    set_workers(K, Workers),
    gproc:reg_shared(?POOL_CUR(Pool), Size).

valid_type(T) when T==round_robin; T==hash; T==random; T==direct; T==claim ->
    true;
valid_type(_) ->
    error(invalid_type).

set_pool_size_(K, Sz, Workers) ->
    {_, Type} = gproc:get_value(K, shared),
    case length(Workers) of
        Sz ->
            set_workers(K, Workers);
        Len when Len > Sz ->
            Workers1 = lists:sublist(Workers, 1, Sz),
            set_workers(K, Workers1);
        Len when Len < Sz ->
            Workers1 = Workers ++ lists:seq(Len+1, Sz),
            set_workers(K, Workers1)
    end,
    gproc:set_value_shared(K, {Sz, Type}).

delete_(Pool) ->
    K = ?POOL(Pool),
    Ws = get_workers_(K),
    case [1 || {_,_} <- Ws] of
        [] ->
            gproc:unreg_shared(K),
            gproc:unreg_shared(?POOL_CUR(Pool));
        [_|_] ->
            error(not_empty)
    end.

force_delete_(Pool) ->
    Props = gproc:select({l,p}, [{ {?POOL(Pool), '_', '_'}, [], ['$_']}]),
    Cur = gproc:select({l,c}, [{ {?POOL_CUR(Pool), '_', '_'}, [], ['$_']}]),
    Workers = gproc:select(
                {l,c}, [{ {?POOL_WRK(Pool,'_'), '_', '_'}, [], ['$_']}]),
    Names = find_names(Pool, '_'),
    lists:foreach(
      fun({Key, Pid, _}) when Pid == self() -> gproc:unreg(Key);
         ({_, Pid, _}) when is_pid(Pid) -> exit(Pid, kill)
      end, Names),
    [gproc:unreg_shared(W) || {W,shared,_} <- Cur ++ Props ++ Workers],
    true.

find_names(Pool, Pid) ->
    gproc:select(
      {l,n}, [{ {{n,l,[?MODULE,Pool,Pid,'_']}, '_', '_'}, [], ['$_']}]).

add_worker_(Pool, Name) ->
    K = ?POOL(Pool),
    {Sz, Type} = gproc:get_value(K, shared),
    AutoSz = gproc:get_attribute(K, shared, auto_size),
    Ws0 = get_workers_(K),
    {N,Ws1} =
        case lists:keymember(Name, 1, Ws0) of
            false ->
                case find_slot(Name, K, Ws0, Sz, Type, AutoSz) of
                    {_, _} = Res ->
                        Res;
                    false ->
                        error(pool_full)
                end;
            true ->
                error(exists)
        end,
    if N > Sz ->
            set_pool_size_(K, N, Ws1); % also calls set_workers/2
       true ->
            %% size not changed
            set_workers(K, Ws1)
    end,
    reg_worker(Pool, Name, N),
    N.

add_worker_(Pool, Name, Pos) ->
    K = ?POOL(Pool),
    {Sz, _} = gproc:get_value(K, shared),
    Ws0 = get_workers_(K),
    if Pos > Sz ->
            case gproc:get_attribute(K, shared, auto_size) of
                true ->
                    Ws1 = Ws0 ++ lists:seq(Sz+1,Pos-1) ++ [{Name, Pos}],
                    set_pool_size_(K, Pos, Ws1);
                false ->
                    error(out_of_range)
            end;
       true ->
            case lists:nth(Pos, Ws0) of
                {_,_} -> error(exists);
                P when is_integer(P) ->
                    Ws1 = set_pos(Pos, Ws0, {Name, Pos}),
                    set_workers(K, Ws1)
            end
    end,
    reg_worker(Pool, Name, Pos),
    Pos.

reg_worker(Pool, Name, Pos) ->
    gproc:reg_shared(Wrk = ?POOL_WRK(Pool, Name), 0),
    gproc:set_attributes_shared(Wrk, [{n, Pos}]).

remove_worker_(Pool, Name) ->
    case whereis_worker(Pool, Name) of
        Pid when is_pid(Pid) ->
            error({worker_connected, Pid});
        undefined ->
            do_remove_worker_(Pool, Name)
    end.

do_remove_worker_(Pool, Name) ->
    K = ?POOL(Pool),
    Ws0 = get_workers_(K),
    Ws1 = del_slot(Name, Ws0),
    gproc:unreg_shared(?POOL_WRK(Pool, Name)),
    case (NewLen = length(Ws1)) - length(Ws0) of
        0 -> ok;
        Diff when Diff < 0 ->
            {_, Type} = gproc:get_value(K, shared),
            gproc:set_value_shared(K, {NewLen, Type})
    end,
    gproc:set_attributes_shared(K, [{workers, Ws1}]),
    ok.

del_slot(Name, [{Name,_}]) ->
    [];
del_slot(Name, [{Name, Pos}|T]) ->
    [Pos|T];
del_slot(Name, [H|T]) ->
    [H|del_slot(Name, T)].

find_slot(Name, _, [], Sz, _, Auto) ->
    case {Sz, Auto} of
        {0, false} -> false;
        {_, _} ->
            {1, [{Name, 1}]}
    end;
find_slot(Name, Key, Workers, Sz, Type, AutoSz) ->
    case get_strategy(Key, Type) of
        packed ->
            find_slot_packed(Name, Workers, AutoSz);
        sparse ->
            find_slot_sparse(Name, Workers, Sz, AutoSz)
    end.
%%     find_slot(Name, Key, Workers, Sz, Type, AutoSz, Strategy).
%% find_slot(Name, []) ->
%%     {1, [{Name, 1}]};
%% find_slot(Name, Slots) ->
%%     find_slot(Name, Slots, []).

get_last_worker_n(Ws) ->
    get_last_worker_n(Ws, 0, 1).

get_last_worker_n([{_,_}|T], _, P) ->
    get_last_worker_n(T, P, P+1);
get_last_worker_n([H|T], Last, P) when is_integer(H) ->
    get_last_worker_n(T, Last, P+1);
get_last_worker_n([], Last, _) ->
    Last.

find_slot_packed(Name, Workers, AutoSz) ->
    find_slot_packed(Name, Workers, AutoSz, []).

find_slot_packed(Name, [N|T], _, Acc) when is_integer(N) -> % empty slot
    {N, lists:reverse(Acc) ++ [{Name, N}|T]};
find_slot_packed(Name, [{_,Prev} = Last], true, Acc) ->  % last elem; expand pool
    New = Prev+1,
    {New, lists:reverse([{Name, New}, Last|Acc])};
find_slot_packed(_, [_], false, _) ->
    false;
find_slot_packed(Name, [{_,_} = H|T], Auto, Acc) ->
    find_slot_packed(Name, T, Auto, [H|Acc]).

find_slot_sparse(Name, Ws, Sz, Auto) ->
    %% Collect the position of the first and last filled slots, as well as
    %% the largest gap between filled slots
    case lists:foldl(
           fun(N, {Prev, StartP, First, Last, Max, MaxP}) when is_integer(N) ->
                   case Prev+1 of
                       Gap when Gap > Max ->
                           {Gap, StartP, First, Last, Gap, StartP};
                       Gap ->
                           {Gap, StartP, First, Last, Max, MaxP}
                   end;
              (N, []) when is_integer(N) ->
                   %% skip
                   [];
              ({_, Pos}, []) ->
                   {0, Pos, _First = Pos, _Last = Pos, 0, 0};
              ({_, Pos}, {Prev, StartP, First, _PrevLast, Max, MaxP}) ->
                   if Prev > Max ->
                           {0, Pos, First, Pos, Prev, StartP};
                      true ->
                           {0, Pos, First, Pos, Max, MaxP}
                   end
           end, [], Ws) of
        [] ->
            %% all empty slots
            case {Sz, Auto} of
                {0, false} ->
                    false;
                {0, true} ->
                    {1, [{Name, 1}]};
                {_, _} when is_integer(Sz), Sz > 0 ->
                    {1, [{Name, 1}|tl(Ws)]}
            end;
        {_, _, 1, Last, 0, _} ->
            %% Pool full
            if Auto ->
                    NewPos = Last + 1,
                    {NewPos, Ws ++ [{Name, NewPos}]};
               true ->
                    false
            end;
        {_, _, First, Last, MaxGap, StartPos} ->
            WrapGap = (Sz - Last) + First - 1,
            NewPos = if WrapGap >= MaxGap ->
                             (Last + (WrapGap div 2) + 1) rem (Sz+1);
                        true ->
                             (StartPos + (MaxGap div 2) + 1) rem (Sz+1)
                     end,
            {NewPos, set_pos(NewPos, Ws, {Name, NewPos})}
    end.

set_pos(P, L, X) when P > 0, is_list(L) ->
    set_pos(P, 1, L, X).

set_pos(P, P, [_|T], X) ->
    [X|T];
set_pos(P, C, [H|T], X) when C < P ->
    [H|set_pos(P, C+1, T, X)].

get_workers_(K) ->
    case gproc:get_attribute(K, shared, workers) of
        undefined ->
            [];
        L when is_list(L) ->
            L
    end.

set_workers(K, L) when is_list(L) ->
    gproc:set_attributes_shared(K, [{workers, L}]).

get_strategy(Key, Type) ->
    Default = case Type of
                  round_robin -> packed;
                  random      -> sparse;
                  hash        -> sparse;
                  direct      -> packed;
                  claim       -> packed
              end,
    attribute(Key, fill_strategy, Default).

attribute(Key, A, Default) ->
    case gproc:get_attribute(Key, shared, A) of
        undefined -> Default;
        Value     -> Value
    end.

incr(Pool, Incr, Sz) ->
    gproc:update_counter(?POOL_CUR(Pool), shared, {Incr, Sz, 1}).

%% find_worker(Pool, Name) ->
%%     case gproc:select(n, [{ {{n, l, {?MODULE, Pool, '_'}}, '_', Name},
%%                        [], ['$_'] }]) of
%%      [] ->
%%          undefined;
%%      [{{n,l,{?MODULE,_,N}}, Pid, _}] ->
%%          {N, Pid}
%%     end.

%% ============================= Test code ===========================

%% @private
test(N) when N > 0 ->
    test(N, round_robin, []).

%% @private
test(N, Type, Opts) when Type==round_robin;
                         Type==random;
                         Type==hash;
                         Type==direct;
                         Type==claim ->
    P = ?LINE,
    setup_test_pool(P, Type, Opts),
    try timer:tc(?MODULE, f(Type), [N, P])
    after
        remove_test_pool(P)
    end.

ptest(N, I, Type, Opts) ->
    P = ?LINE,
    setup_test_pool(P, Type, Opts),
    F = f(Type),
    Pids =
        [spawn_monitor(fun() -> exit({ok, timer:tc(?MODULE, F, [I, P])}) end)
         || _ <- lists:seq(1, N)],
    try collect(Pids)
    after
        remove_test_pool(P)
    end.

collect(Pids) ->
    Results = [receive
                   {'DOWN', Ref, _, _, Reason} ->
                       Reason
               end || {_, Ref} <- Pids],
    {Times, Avgs} = lists:foldr(fun({ok, {T, Avg}}, {A,B}) ->
                                        {[T|A], [Avg|B]} end,
                                {[],[]}, Results),
    {Times, lists:sum(Times)/length(Times),
     lists:sum(Avgs)/length(Avgs)}.

f(Type) when Type==hash; Type==direct ->
    test_run1;
f(Type) when Type==claim ->
    test_run2;
f({empty,_}) ->
    test_run0;
f(_) ->
    test_run.



%% @private
setup_test_pool(P, Type, Opts) ->
    setup_test_pool(P, Type, Opts, test_workers()).

setup_test_pool(P, Type0, Opts, Workers) ->
    Type = case Type0 of {_, T} -> T; T when is_atom(T) -> T end,
    new(P, Type, Opts),
    [begin R = add_worker(P, W),
           io:fwrite("add_worker(~p, ~p) -> ~p; Ws = ~p~n",
                     [P, W, R, get_workers_(?POOL(P))]),
           connect_worker(P, W)
     end || W <- Workers].


%% @private
remove_test_pool(P) ->
    io:fwrite("worker stats (~p):~n"
              "~p~n", [P, gproc:select(
                            {l,c},
                            [{ {{c,l,{?MODULE,P,w,'$1'}},'_','$2'}, [],
                               [{{'$1','$2'}}] }])]),
    [begin disconnect_worker(P, W),
           remove_worker(P, W)
     end || W <- test_workers()],
    delete(P).

test_workers() -> [a,b,c,d,e,f].

%% @private
test_run(N, P) ->
    test_run(N, P, 0, 0).

test_run(N, P, S, M) when N > 0 ->
    {T, Worker} = timer:tc(?MODULE, pick, [P]),
    true = (Worker =/= false),
    log(Worker),
    timer:sleep(rand:uniform(50)),
    test_run(N-1, P, S+T, M+1);
test_run(_, _, S, M) ->
    S/M.

%% @private
test_run1(N, P) ->
    test_run1(N, P, 0, 0).
test_run1(N, P, S, M) when N > 0 ->
    {T, Worker} = timer:tc(?MODULE, pick, [P, N]),
    true = (Worker =/= false),
    log(Worker),
    timer:sleep(rand:uniform(50)),
    test_run1(N-1, P, S+T, M+1);
test_run1(_, _, S, M) ->
    S/M.

%% @private
test_run2(N, P) ->
    test_run2(N, P, fun(K,_) ->
			    R = log(K),
			    timer:sleep(rand:uniform(50)),
			    R
		    end, 0, 0).

test_run2(N, P, F, S, M) when N > 0 ->
    {T, {true, _}} = timer:tc(?MODULE, claim, [P, F, {busy_wait, 5000}]),
    test_run2(N-1, P, F, S+T, M+1);
test_run2(_, _, _, S, M) ->
    S/M.

test_run0(N, X) when N > 0 ->
    test_run0(N-1, X);
test_run0(_, _) ->
    ok.
