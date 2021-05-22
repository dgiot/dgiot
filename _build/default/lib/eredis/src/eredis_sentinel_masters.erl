%% ADT for sentinel masters data.
%% Keeps current masters host/port and list of subscribers.
%% Notifies subscribers when master changes.

-module(eredis_sentinel_masters).
-author("Mikl Kurkov <mkurkov@gmail.com>").

%% API
-export([new/0, update/4, subscribe/3, unsubscribe/2]).

%% Records
-record(master, {
          name :: master_name(),
          host :: master_host(),
          port :: master_port(),
          pids :: [pid()]}).

%% Types
-type master_host() :: string().
-type master_port() :: integer().
-type master_name() :: atom().
-type masters() :: [#master{}].

%%% API ---------------------------------------------------------------

%% @doc Masters initialization
-spec new() -> {ok, masters()}.
new() ->
    {ok,[]}.

%% @doc Add new master or update if it already exists
-spec update(masters(), master_name(), master_host(), master_port()) -> {ok,masters()}.
update(Masters, MasterName, Host, Port)
  when is_list(Masters),
       is_atom(MasterName),
       is_list(Host),
       is_integer(Port)
  ->
    NewMaster = case find(Masters, MasterName) of
        {ok, Master} ->
            update_master(Master, Host, Port);
        undefined ->
            new_master(MasterName, Host, Port)
    end,
    {ok, set_master(Masters, NewMaster)}.

%% @doc Subscribe process to master updates.
-spec subscribe(masters(), master_name(), pid()) -> {ok, masters()} | {error, no_master_found}.
subscribe(Masters, MasterName, Pid)
  when is_list(Masters), is_atom(MasterName), is_pid(Pid) ->
    case find(Masters, MasterName) of
        {ok, Master} ->
            NewMaster = add_pid(Master, Pid),
            {ok, set_master(Masters, NewMaster)};
        undefined ->
            {error, no_master_found}
    end.

%% @doc Unsubscribe process from all masters.
-spec unsubscribe(masters(), pid()) -> {ok, masters()}.
unsubscribe(Masters, Pid)
  when is_list(Masters), is_pid(Pid) ->
    RemovePid = fun(M) -> rm_pid(M, Pid) end,
    {ok, lists:map(RemovePid, Masters)}.


%%% Internal ----------------------------------------------------------

new_master(MasterName, Host, Port) ->
    #master{name = MasterName, host = Host, port = Port, pids =[]}.

find(Masters,MasterName) ->
    case lists:keysearch(MasterName, #master.name, Masters) of
        {value, Master} ->
            {ok, Master};
        false ->
            undefined
    end.

set_master(Masters, Master) ->
    lists:keystore(Master#master.name, #master.name, Masters, Master).

update_master(#master{host=Host, port=Port}=Master, Host, Port) ->
    Master;
update_master(Master, Host, Port) ->
    notify_pids(Master#master{host=Host,port=Port}).

-spec notify_pids(#master{}) -> #master{}.
notify_pids(#master{pids=Pids, name=Name, host=Host, port=Port}=Master) ->
    Message = {sentinel, {reconnect, Name, Host, Port}},
    NewPids = [ begin Pid ! Message, Pid end || Pid <- Pids, is_process_alive(Pid) ],
    Master#master{pids=NewPids}.

add_pid(#master{pids=Pids} = Master, Pid) ->
    Master#master{pids = lists:umerge(Pids, [Pid])}.

rm_pid(#master{pids=Pids} = Master, Pid) ->
    Master#master{pids = Pids -- [Pid]}.
