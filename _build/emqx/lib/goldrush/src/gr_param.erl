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

-module(gr_param).

-behaviour(gen_server).

%% API
-export([start_link/1, 
         list/1, insert/2, 
         lookup/2, lookup_element/2,
         info/1, info_size/1, transform/1]).

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

info_size(Server) ->
    case (catch gen_server:call(Server, info_size)) of
        {'EXIT', _Reason} ->
            info_size(gr_manager:wait_for_pid(Server));
        Else -> Else
    end.

insert(Server, Term) ->
    case (catch gen_server:call(Server, {insert, Term})) of
        {'EXIT', _Reason} ->
            insert(gr_manager:wait_for_pid(Server), Term);
        Else -> Else
    end.

lookup(Server, Term) ->
    case (catch gen_server:call(Server, {lookup, Term})) of
        {'EXIT', _Reason} ->
            lookup(gr_manager:wait_for_pid(Server), Term);
        Else -> Else
    end.

lookup_element(Server, Term) ->
    case (catch gen_server:call(Server, {lookup_element, Term})) of
        {'EXIT', _Reason} ->
            lookup_element(gr_manager:wait_for_pid(Server), Term);
        Else -> Else
    end.

info(Server) ->
    case (catch gen_server:call(Server, info)) of
        {'EXIT', _Reason} ->
            info(gr_manager:wait_for_pid(Server));
        Else -> Else
    end.

%% @doc Transform Term -> Key to Key -> Term
transform(Server) ->
    case (catch gen_server:call(Server, transform)) of
        {'EXIT', _Reason} ->
            transform(gr_manager:wait_for_pid(Server));
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
handle_call(Call, From, State) when is_atom(Call), Call =:= list; 
                                     Call =:= info; Call =:= info_size;
                                     Call =:= transform ->
    TableId = State#state.table_id,
    Waiting = State#state.waiting,
    case TableId of
        undefined -> {noreply, State#state{waiting=[{Call, From}|Waiting]}};
        _ when Call =:= list -> 
            {reply, handle_list(TableId), State};
        _ when Call =:= info -> 
            {reply, handle_info(TableId), State};
        _ when Call =:= info_size -> 
            {reply, handle_info_size(TableId), State};
        _ when Call =:= transform -> 
            {reply, handle_transform(TableId), State}
    end;

handle_call({Call, Term}, From, State) when is_atom(Call), Call =:= insert; 
                                              Call =:= lookup; 
                                              Call =:= lookup_element ->
    TableId = State#state.table_id,
    Waiting = State#state.waiting,
    case TableId of
        undefined -> 
            {noreply, State#state{waiting=[{{Call, Term}, From}|Waiting]}};
        _ when Call =:= insert -> 
            {reply, handle_insert(TableId, Term), State};
        _ when Call =:= lookup -> 
            {reply, handle_lookup(TableId, Term), State};
        _ when Call =:= lookup_element -> 
            {reply, handle_lookup_element(TableId, Term), State}
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
        info ->
            handle_info(TableId);
        info_size ->
            handle_info_size(TableId);
        transform ->
            handle_transform(TableId);
        {insert, Term} ->
            handle_insert(TableId, Term);
        {lookup, Term} ->
            handle_lookup(TableId, Term);
        {lookup_element, Term} ->
            handle_lookup_element(TableId, Term)
    end.


handle_list(TableId) ->
    ets:tab2list(TableId).

handle_info(TableId) ->
    ets:info(TableId).

handle_info_size(TableId) ->
    ets:info(TableId, size).

handle_transform(TableId) ->
    ParamsList = [{K, V} || {V, K} <- ets:tab2list(TableId)],
    ets:delete_all_objects(TableId),
    ets:insert(TableId, ParamsList).

handle_insert(TableId, Term) ->
    ets:insert(TableId, Term).

handle_lookup(TableId, Term) ->
    ets:lookup(TableId, Term).

handle_lookup_element(TableId, Term) ->
    ets:lookup_element(TableId, Term, 2).

