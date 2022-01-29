%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service dgiot.Greeter.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dgiot_greeter_bhvr).

-callback say_hello(dgiot_dlink_pb:hello_request(), grpc:metadata())
    -> {ok, dgiot_dlink_pb:hello_reply(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

