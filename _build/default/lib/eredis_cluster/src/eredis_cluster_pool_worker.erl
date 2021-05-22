-module(eredis_cluster_pool_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API.
-export([start_link/1]).
-export([query/2, is_connected/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {conn, host, port, database, password}).

-define(RECONNECT_TIME, 100).

is_connected(Pid) ->
    gen_server:call(Pid, is_connected).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Hostname = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    DataBase = proplists:get_value(database, Args, 0),
    Password = proplists:get_value(password, Args, ""),
    Options = proplists:get_value(options, Args, []),
    erlang:put(options, Options),
    process_flag(trap_exit, true),
    Conn = start_connection(Hostname, Port, DataBase, Password, Options),
    {ok, #state{conn = Conn,
                host = Hostname,
                port = Port,
                database = DataBase,
                password = Password}}.

query(Worker, Commands) ->
    gen_server:call(Worker, {'query', Commands}).

handle_call({'query', _}, _From, #state{conn = undefined} = State) ->
    {reply, {error, no_connection}, State};
handle_call({'query', [[X|_]|_] = Commands}, _From, #state{conn = Conn} = State)
    when is_list(X); is_binary(X) ->
    {reply, eredis:qp(Conn, Commands), State};
handle_call({'query', Command}, _From, #state{conn = Conn} = State) ->
    {reply, eredis:q(Conn, Command), State};
handle_call(is_connected, _From, #state{conn = Conn}= State) ->
    {reply, Conn =/= undefined andalso is_process_alive(Conn), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reconnect, #state{host = Hostname,
                              port = Port,
                              database = DataBase,
                              password = Password} = State) ->
    Options = case erlang:get(options) of
        undefined -> [];
        Options0 -> Options0
    end,
    Conn = start_connection(Hostname, Port, DataBase, Password, Options),
    {noreply, State#state{conn = Conn}};

handle_info({'EXIT', Pid, _Reason}, #state{conn = Pid} = State) ->
    erlang:send_after(?RECONNECT_TIME, self(), reconnect),
    {noreply, State#state{conn = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=undefined}) ->
    ok;
terminate(_Reason, #state{conn=Conn}) ->
    ok = eredis:stop(Conn),
    ok.

code_change(_, State, _Extra) ->
    {ok, State}.

start_connection(Hostname, Port, DataBase, Password, Options) ->
    case eredis:start_link(Hostname, Port, DataBase, Password, no_reconnect, 5000, Options) of
        {ok,Connection} ->
            Connection;
        _ ->
            erlang:send_after(?RECONNECT_TIME, self(), reconnect),
            undefined
    end.
