%%%-------------------------------------------------------------------
%% @doc Client module for grpc service dgiot.Greeter.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dgiot_greeter_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpc/include/grpc.hrl").

-define(SERVICE, 'dgiot.Greeter').
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
    grpc_client:unary(?DEF(<<"/dgiot.Greeter/SayHello">>,
                           hello_request, hello_reply, <<"dgiot.HelloRequest">>),
                      Req, Metadata, Options).

