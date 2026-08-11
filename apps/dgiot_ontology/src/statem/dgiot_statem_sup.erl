%%--------------------------------------------------------------------
%% dgiot_statem_sup — simple_one_for_one supervisor
%%
%% Manages gen_statem instances. One child per device.
%%--------------------------------------------------------------------
-module(dgiot_statem_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2, start_child/3, stop_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(DeviceId, ModelId) ->
    start_child(DeviceId, ModelId, #{}).

start_child(DeviceId, ModelId, Data) ->
    supervisor:start_child(?MODULE, [DeviceId, ModelId, Data]).

stop_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1000,
                 period => 60},
    ChildSpec = #{id => dgiot_statem,
                  start => {dgiot_statem, start_link, []},
                  restart => transient,
                  shutdown => 5000,
                  type => worker,
                  modules => [dgiot_statem]},
    {ok, {SupFlags, [ChildSpec]}}.
