%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service dgiot.Dlink.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dgiot_dlink_bhvr).

-callback payload(dgiot_dlink_pb:payload_request(), grpc:metadata())
    -> {ok, dgiot_dlink_pb:payload_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

