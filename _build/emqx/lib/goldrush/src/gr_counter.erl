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

-module(gr_counter).

-behaviour(gen_server).

%% API
-export([start_link/1, 
         list/1, lookup_element/2,
         insert_counter/3,
         update_counter/3, reset_counters/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {table_id, waiting=[]}).

%%%===================================================================
%%% API
%%%===================================================================
list(Server) ->
    case (catch gen_server:call(Server, list)) of
        {'EXIT', _Reason} ->
            list(gr_manager:wait_for_pid(Server));
        Else -> Else
    end.

lookup_element(Server, Term) ->
    case (catch gen_server:call(Server, {lookup_element, Term})) of
        {'EXIT', _Reason} ->
            lookup_element(gr_manager:wait_for_pid(Server), Term);
        Else -> Else
    end.

insert_counter(Server, Counter, Value) when is_atom(Server) ->
    case whereis(Server) of
        undefined -> 
            insert_counter(gr_manager:wait_for_pid(Server), Counter, Value);
        Pid -> 
            case erlang:is_process_alive(Pid) of
                true ->
                    insert_counter(Pid, Counter, Value);
                false ->
                    ServerPid = gr_manager:wait_for_pid(Server),
                    insert_counter(ServerPid, Counter, Value)
            end
    end;
insert_counter(Server, Counter, Value) when is_pid(Server) ->
    case (catch gen_server:call(Server, {insert_counter, Counter, Value})) of
        {'EXIT', _Reason} ->
            insert_counter(gr_manager:wait_for_pid(Server), Counter, Value);
        Else -> Else
    end.

update_counter(Server, Counter, Value) when is_atom(Server) ->
    case whereis(Server) of
        undefined -> 
            update_counter(gr_manager:wait_for_pid(Server), Counter, Value);
        Pid -> 
            case erlang:is_process_alive(Pid) of
                true ->
                    update_counter(Pid, Counter, Value);
                false ->
                    ServerPid = gr_manager:wait_for_pid(Server),
                    update_counter(ServerPid, Counter, Value)
            end
    end;
update_counter(Server, Counter, Value) when is_pid(Server) ->
    gen_server:cast(Server, {update, Counter, Value}).

reset_counters(Server, Counter) ->
    case (catch gen_server:call(Server, {reset_counters, Counter})) of
        {'EXIT', _Reason} ->
            reset_counters(gr_manager:wait_for_pid(Server), Counter);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Name) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{}}.

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
handle_call(list=Call, From, State) ->
    TableId = State#state.table_id,
    Waiting = State#state.waiting,
    case TableId of
        undefined -> {noreply, State#state{waiting=[{Call, From}|Waiting]}};
        _ -> {reply, lists:sort(handle_list(TableId)), State}
    end;
handle_call({lookup_element, Term}=Call, From, State) ->
    TableId = State#state.table_id,
    Waiting = State#state.waiting,
    case TableId of
        undefined -> {noreply, State#state{waiting=[{Call, From}|Waiting]}};
        _ -> {reply, handle_lookup_element(TableId, Term), State}
    end;
handle_call({insert_counter, Counter, Value}, From, State) ->
    Term = [{Counter, Value}],
    Call = {insert, Term},
    TableId = State#state.table_id,
    Waiting = State#state.waiting,
    case TableId of
        undefined -> {noreply, State#state{waiting=[{Call, From}|Waiting]}};
        _ -> {reply, handle_insert(TableId, Term), State}
    end;
handle_call({reset_counters, Counter}, From, State) ->
    Term = case Counter of
        _ when is_list(Counter) -> 
            [{Item, 0} || Item <- Counter];
        _ when is_atom(Counter) -> 
            [{Counter, 0}]
    end,
    Call = {insert, Term},
    TableId = State#state.table_id,
    Waiting = State#state.waiting,
    case TableId of
        undefined -> {noreply, State#state{waiting=[{Call, From}|Waiting]}};
        _ -> {reply, handle_insert(TableId, Term), State}
    end;
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
handle_cast({update, Counter, Value}=Call, State) ->
    TableId = State#state.table_id,
    Waiting = State#state.waiting,
    State2 = case TableId of
        undefined -> State#state{waiting=[Call|Waiting]};
        _ -> _ = handle_update_counter(TableId, Counter, Value), 
             State
    end,
    {noreply, State2};
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
handle_info({'ETS-TRANSFER', TableId, _Pid, _Data}, State) ->
    _ = [ gen_server:reply(From, perform_call(TableId, Call)) 
      || {Call, From} <- State#state.waiting ],
    _ = [ handle_update_counter(TableId, Counter, Value) 
      || {update, Counter, Value} <- State#state.waiting ],
    {noreply, State#state{table_id=TableId, waiting=[]}};
handle_info(_Info, State) ->
    {noreply, State}.

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

perform_call(TableId, Call) ->
    case Call of
        list ->
            handle_list(TableId);
        {insert, Term} ->
            handle_insert(TableId, Term);
        {lookup_element, Term} ->
            handle_lookup_element(TableId, Term)
    end.

handle_list(TableId) ->
    ets:tab2list(TableId).

handle_update_counter(TableId, Counter, Value) ->
    ets:update_counter(TableId, Counter, Value).

handle_insert(TableId, Term) ->
    ets:insert(TableId, Term).

handle_lookup_element(TableId, Term) ->
    ets:lookup_element(TableId, Term, 2).
