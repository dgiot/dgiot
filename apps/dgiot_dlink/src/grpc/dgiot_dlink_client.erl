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

-spec payload(dgiot_dlink_pb:payload_request())
    -> {ok, dgiot_dlink_pb:payload_response(), grpc:metadata()}
     | {error, term()}.
payload(Req) ->
    payload(Req, #{}, #{}).

-spec payload(dgiot_dlink_pb:payload_request(), grpc:options())
    -> {ok, dgiot_dlink_pb:payload_response(), grpc:metadata()}
     | {error, term()}.
payload(Req, Options) ->
    payload(Req, #{}, Options).

-spec payload(dgiot_dlink_pb:payload_request(), grpc:metadata(), grpc_client:options())
    -> {ok, dgiot_dlink_pb:payload_response(), grpc:metadata()}
     | {error, term()}.
payload(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/dgiot.Dlink/Payload">>,
                           payload_request, payload_response, <<"dgiot.PayloadRequest">>),
                      Req, Metadata, Options).

