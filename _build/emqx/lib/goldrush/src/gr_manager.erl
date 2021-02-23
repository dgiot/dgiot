%% Copyright (c) 2013, Pedram Nimreezi <deadzen@deadzen.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


%% @doc Process table manager for goldrush.
%%
%% Manager responsible for the processes, which serve as heir of the 
%% {@link gr_counter:start_link/0. <em>Counter</em>} and
%% {@link gr_param:start_link/0. <em>Param</em>} ets table processes.
%% This process creates the table and initial data then assigns itself
%% to inherit the ets table if any process responsible for it is killed.
%% It then waits to give it back while that process is recreated by its 
%% supervisor.
-module(gr_manager).
-behaviour(gen_server).

%% API
-export([start_link/3, wait_for_pid/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {table_id :: ets:tab(), managee :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================

%% Setup the initial data for the ets table
-spec setup(atom() | pid(), term()) -> ok.
setup(Name, Data) ->
    gen_server:cast(Name, {setup, Data}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Name, Managee, Data) -> {ok, Pid} | ignore | 
%%                                          {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Managee, Data) ->
    gen_server:start_link({local, Name}, ?MODULE, [Managee, Data], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Managee, Data]) ->
    process_flag(trap_exit, true),
    setup(self(), Data),
    {ok, #state{managee=Managee}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = {error, unhandled_message},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({setup, Data}, State = #state{managee=Managee}) ->
    ManageePid = whereis(Managee),
    link(ManageePid),
    TableId = ets:new(?MODULE, [set, private]),
    ets:insert(TableId, Data),
    ets:setopts(TableId, {heir, self(), Data}),
    ets:give_away(TableId, ManageePid, Data),
    {noreply, State#state{table_id=TableId}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info({'ETS-TRANSFER', TableId, _Pid, Data}, State = #state{managee=Managee}) ->
    ManageePid = wait_for_pid(Managee),
    link(ManageePid),
    ets:give_away(TableId, ManageePid, Data),
    {noreply, State#state{table_id=TableId}}.

%% @doc Wait for a registered process to be associated to a process identifier.
%% @spec wait_for_pid(Managee) -> ManageePid
-spec wait_for_pid(atom()) -> pid().
wait_for_pid(Managee) when is_pid(Managee) -> 
    Managee;
wait_for_pid(Managee) when is_atom(Managee), Managee =/= undefined -> 
    case whereis(Managee) of
        undefined -> 
            timer:sleep(1),
            wait_for_pid(Managee);
        ManageePid -> ManageePid
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



