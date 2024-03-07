-module(route_guide).

-behaviour(supervisor).
-behaviour(application).

-export([start/2, stop/1]).
-export([init/1]).

-export([start_services/0, start_client_channel/0,
         stop_services/0, stop_client_channel/0]).

%%--------------------------------------------------------------------
%% APIs

-define(SERVER_NAME, route_guide).
-define(CHANN_NAME,  channel1).

start_services() ->
    Services = #{protos => [route_guide_pb],
                 services => #{'routeguide.RouteGuide' => route_guide_svr}
                },
    Options = [],
    {ok, _} = grpc:start_server(?SERVER_NAME, 10000, Services, Options),
    io:format("Start service ~s on 10000 successfully!~n", [?SERVER_NAME]).

start_client_channel() ->
    ClientOps = #{},
    SvrAddr = "http://127.0.0.1:10000",
    {ok, _} = grpc_client_sup:create_channel_pool(
                ?CHANN_NAME,
                SvrAddr,
                ClientOps
               ),
    io:format("Start client channel ~s for ~s successfully!~n~n"
              "Call the 'routeguide_route_guide_client' module exported functions "
              "to use it. e.g:~n"
              "  routeguide_route_guide_client:get_feature(#{latitude => 1"
              ", longitude => 1}, #{channel => channel1}).~n",
              [?CHANN_NAME, SvrAddr]).

stop_services() ->
    grpc:stop_server(?SERVER_NAME).

stop_client_channel() ->
    grpc_client_sup:stop_channel_pool(?CHANN_NAME).

%%--------------------------------------------------------------------
%% APIs for application

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% callbacks for supervisor

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
