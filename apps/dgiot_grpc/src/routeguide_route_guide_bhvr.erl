%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service routeguide.RouteGuide.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(routeguide_route_guide_bhvr).

-callback get_feature(route_guide_pb:point(), grpc:metadata())
    -> {ok, route_guide_pb:feature(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback list_features(grpc_stream:stream(), grpc:metadata())
    -> {ok, grpc_stream:stream()}.

-callback record_route(grpc_stream:stream(), grpc:metadata())
    -> {ok, grpc_stream:stream()}.

-callback route_chat(grpc_stream:stream(), grpc:metadata())
    -> {ok, grpc_stream:stream()}.

