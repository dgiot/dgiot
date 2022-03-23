-module(dgiot_httpc_sup).

-behaviour(supervisor).

-include("dgiot_bridge.hrl").

%% API
-export([
    start/1,
    childSpec/1
]).
-export([start_link/1, init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start(#{<<"channelid">> := ChannelId} = Args) ->
    supervisor:start_child(?DGIOT_SUP(ChannelId), [Args]).

childSpec(ChannelId) ->
    [?CHILD(dgiot_httpc_sup, supervisor, [?DGIOT_SUP(ChannelId)])].

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Child = [
        {dgiot_httpc_worker, {dgiot_httpc_worker, start_link, []}, transient, 5000, worker, [dgiot_httpc_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.