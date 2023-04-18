%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service dgiot.Dlink.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dgiot_dlink_bhvr).

-callback login(dgiot_dlink_pb:login_request(), grpc:metadata())
    -> {ok, dgiot_dlink_pb:login_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback logout(dgiot_dlink_pb:logout_request(), grpc:metadata())
    -> {ok, dgiot_dlink_pb:logout_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback payload(dgiot_dlink_pb:payload_request(), grpc:metadata())
    -> {ok, dgiot_dlink_pb:payload_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

