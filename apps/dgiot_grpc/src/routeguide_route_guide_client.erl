%%%-------------------------------------------------------------------
%% @doc Client module for grpc service routeguide.RouteGuide.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(routeguide_route_guide_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpc/include/grpc.hrl").

-define(SERVICE, 'routeguide.RouteGuide').
-define(PROTO_MODULE, 'route_guide_pb').
-define(MARSHAL(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Path, Req, Resp, MessageType),
        #{path => Path,
          service =>?SERVICE,
          message_type => MessageType,
          marshal => ?MARSHAL(Req),
          unmarshal => ?UNMARSHAL(Resp)}).

-spec get_feature(route_guide_pb:point())
    -> {ok, route_guide_pb:feature(), grpc:metadata()}
     | {error, term()}.
get_feature(Req) ->
    get_feature(Req, #{}, #{}).

-spec get_feature(route_guide_pb:point(), grpc:options())
    -> {ok, route_guide_pb:feature(), grpc:metadata()}
     | {error, term()}.
get_feature(Req, Options) ->
    get_feature(Req, #{}, Options).

-spec get_feature(route_guide_pb:point(), grpc:metadata(), grpc_client:options())
    -> {ok, route_guide_pb:feature(), grpc:metadata()}
     | {error, term()}.
get_feature(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/routeguide.RouteGuide/GetFeature">>,
                           point, feature, <<"routeguide.Point">>),
                      Req, Metadata, Options).

-spec list_features(grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
list_features(Options) ->
    list_features(#{}, Options).

-spec list_features(grpc:metadata(), grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
list_features(Metadata, Options) ->
    grpc_client:open(?DEF(<<"/routeguide.RouteGuide/ListFeatures">>,
                          rectangle, feature, <<"routeguide.Rectangle">>),
                     Metadata, Options).

-spec record_route(grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
record_route(Options) ->
    record_route(#{}, Options).

-spec record_route(grpc:metadata(), grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
record_route(Metadata, Options) ->
    grpc_client:open(?DEF(<<"/routeguide.RouteGuide/RecordRoute">>,
                          point, route_summary, <<"routeguide.Point">>),
                     Metadata, Options).

-spec route_chat(grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
route_chat(Options) ->
    route_chat(#{}, Options).

-spec route_chat(grpc:metadata(), grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
route_chat(Metadata, Options) ->
    grpc_client:open(?DEF(<<"/routeguide.RouteGuide/RouteChat">>,
                          route_note, route_note, <<"routeguide.RouteNote">>),
                     Metadata, Options).

