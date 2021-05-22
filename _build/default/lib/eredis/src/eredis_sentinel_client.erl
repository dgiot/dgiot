%% Sentinel client connection functions.
%%
-module(eredis_sentinel_client).
-author("Mikl Kurkov <mkurkov@gmail.com>").

-include("eredis.hrl").
-include("eredis_sentinel.hrl").

%% API
-export([start_link/2, start_link/3, stop/1, get_master/2]).

%%% API ---------------------------------------------------------------

start_link(Host, Port) when is_list(Host), is_integer(Port) ->
    start_link(Host, Port, []).

start_link(Host, Port, Opts) when is_list(Host), is_integer(Port), is_list(Opts) ->
    eredis:start_link(Host, Port, undefined, "", no_reconnect, 5000, Opts).

stop(Pid) when is_pid(Pid) ->
    catch eredis:stop(Pid).

get_master(Pid, MasterName) when is_pid(Pid), is_atom(MasterName) ->
    Req = ["SENTINEL", "get-master-addr-by-name", atom_to_list(MasterName)],
    try get_master_response(eredis:q(Pid,Req)) of
        Result ->
            Result
    catch Type:Error ->
       error_logger:error_msg("Sentinel error getting master ~p : ~p:~p", [MasterName, Type, Error]),
       {error, ?SENTINEL_UNREACHABLE}
    end.



%%% Internal ----------------------------------------------------------

get_master_response({ok, [HostBin, PortBin]}) ->
    Host = binary_to_list(HostBin),
    Port = list_to_integer(binary_to_list(PortBin)),
    {ok, {Host, Port}};
get_master_response({ok, undefined}) ->
    {error, ?MASTER_UNKNOWN};
get_master_response({error, <<"IDONTKNOW", _Rest/binary >>}) ->
    {error, ?MASTER_UNREACHABLE}.
