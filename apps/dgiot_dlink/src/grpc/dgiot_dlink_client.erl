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

-spec login(dgiot_dlink_pb:login_request())
    -> {ok, dgiot_dlink_pb:login_response(), grpc:metadata()}
     | {error, term()}.
login(Req) ->
    login(Req, #{}, #{}).

-spec login(dgiot_dlink_pb:login_request(), grpc:options())
    -> {ok, dgiot_dlink_pb:login_response(), grpc:metadata()}
     | {error, term()}.
login(Req, Options) ->
    login(Req, #{}, Options).

-spec login(dgiot_dlink_pb:login_request(), grpc:metadata(), grpc_client:options())
    -> {ok, dgiot_dlink_pb:login_response(), grpc:metadata()}
     | {error, term()}.
login(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/dgiot.Dlink/Login">>,
                           login_request, login_response, <<"dgiot.LoginRequest">>),
                      Req, Metadata, Options).

-spec logout(dgiot_dlink_pb:logout_request())
    -> {ok, dgiot_dlink_pb:logout_response(), grpc:metadata()}
     | {error, term()}.
logout(Req) ->
    logout(Req, #{}, #{}).

-spec logout(dgiot_dlink_pb:logout_request(), grpc:options())
    -> {ok, dgiot_dlink_pb:logout_response(), grpc:metadata()}
     | {error, term()}.
logout(Req, Options) ->
    logout(Req, #{}, Options).

-spec logout(dgiot_dlink_pb:logout_request(), grpc:metadata(), grpc_client:options())
    -> {ok, dgiot_dlink_pb:logout_response(), grpc:metadata()}
     | {error, term()}.
logout(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/dgiot.Dlink/Logout">>,
                           logout_request, logout_response, <<"dgiot.LogoutRequest">>),
                      Req, Metadata, Options).

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

