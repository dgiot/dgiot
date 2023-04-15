%%%-------------------------------------------------------------------
%% @doc Client module for grpc service dgiot.Dlink.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dgiot_dlink_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpc/include/grpc.hrl").

-define(SERVICE, 'dgiot.Dlink').
-define(PROTO_MODULE, 'dgiot_dlink_pb').
-define(MARSHAL(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Path, Req, Resp, MessageType),
        #{path => Path,
          service =>?SERVICE,
          message_type => MessageType,
          marshal => ?MARSHAL(Req),
          unmarshal => ?UNMARSHAL(Resp)}).

-spec say_hello(dgiot_dlink_pb:hello_request())
    -> {ok, dgiot_dlink_pb:hello_reply(), grpc:metadata()}
     | {error, term()}.
say_hello(Req) ->
    say_hello(Req, #{}, #{}).

-spec say_hello(dgiot_dlink_pb:hello_request(), grpc:options())
    -> {ok, dgiot_dlink_pb:hello_reply(), grpc:metadata()}
     | {error, term()}.
say_hello(Req, Options) ->
    say_hello(Req, #{}, Options).

-spec say_hello(dgiot_dlink_pb:hello_request(), grpc:metadata(), grpc_client:options())
    -> {ok, dgiot_dlink_pb:hello_reply(), grpc:metadata()}
     | {error, term()}.
say_hello(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/dgiot.Dlink/SayHello">>,
                           hello_request, hello_reply, <<"dgiot.HelloRequest">>),
                      Req, Metadata, Options).

-spec check(dgiot_dlink_pb:health_check_request())
    -> {ok, dgiot_dlink_pb:health_check_response(), grpc:metadata()}
     | {error, term()}.
check(Req) ->
    check(Req, #{}, #{}).

-spec check(dgiot_dlink_pb:health_check_request(), grpc:options())
    -> {ok, dgiot_dlink_pb:health_check_response(), grpc:metadata()}
     | {error, term()}.
check(Req, Options) ->
    check(Req, #{}, Options).

-spec check(dgiot_dlink_pb:health_check_request(), grpc:metadata(), grpc_client:options())
    -> {ok, dgiot_dlink_pb:health_check_response(), grpc:metadata()}
     | {error, term()}.
check(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/dgiot.Dlink/Check">>,
                           health_check_request, health_check_response, <<"dgiot.HealthCheckRequest">>),
                      Req, Metadata, Options).

-spec watch(grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
watch(Options) ->
    watch(#{}, Options).

-spec watch(grpc:metadata(), grpc_client:options())
    -> {ok, grpc_client:grpcstream()}
     | {error, term()}.
watch(Metadata, Options) ->
    grpc_client:open(?DEF(<<"/dgiot.Dlink/Watch">>,
                          health_check_request, health_check_response, <<"dgiot.HealthCheckRequest">>),
                     Metadata, Options).

