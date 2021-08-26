%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service dgiot.exhook.v1.HookProvider.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(dgiot_exhook_v_1_hook_provider_bhvr).

-callback on_provider_loaded(exhook_pb:provider_loaded_request(), grpc:metadata())
    -> {ok, exhook_pb:loaded_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_provider_unloaded(exhook_pb:provider_unloaded_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_connect(exhook_pb:client_connect_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_connack(exhook_pb:client_connack_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_connected(exhook_pb:client_connected_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_disconnected(exhook_pb:client_disconnected_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_authenticate(exhook_pb:client_authenticate_request(), grpc:metadata())
    -> {ok, exhook_pb:valued_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_check_acl(exhook_pb:client_check_acl_request(), grpc:metadata())
    -> {ok, exhook_pb:valued_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_subscribe(exhook_pb:client_subscribe_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_client_unsubscribe(exhook_pb:client_unsubscribe_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_session_created(exhook_pb:session_created_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_session_subscribed(exhook_pb:session_subscribed_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_session_unsubscribed(exhook_pb:session_unsubscribed_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_session_resumed(exhook_pb:session_resumed_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_session_discarded(exhook_pb:session_discarded_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_session_takeovered(exhook_pb:session_takeovered_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_session_terminated(exhook_pb:session_terminated_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_message_publish(exhook_pb:message_publish_request(), grpc:metadata())
    -> {ok, exhook_pb:valued_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_message_delivered(exhook_pb:message_delivered_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_message_dropped(exhook_pb:message_dropped_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback on_message_acked(exhook_pb:message_acked_request(), grpc:metadata())
    -> {ok, exhook_pb:empty_success(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

