-module(emqx_bridge_nats).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-include("emqx_bridge_nats.hrl").

-export([load/1, unload/0]).
% 
-export([on_client_connected/3,
         on_client_disconnected/4,
         on_client_authenticate/3,
         on_message_publish/2]).

%% Called when the plugin application start
load(Env) ->
    emqx:hook('client.connected', {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    emqx:hook('message.publish', {?MODULE, on_message_publish, [Env]}).

%% Called when the plugin application stop
unload() ->
    emqx:unhook('client.connected', {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
    emqx:unhook('message.publish', {?MODULE, on_message_publish}).


%%--------------------------------------------------------------------
%% Client Lifecircle Hooks
%%--------------------------------------------------------------------


on_client_connected(_ClientInfo = #{clientid := ClientId}, _ConnInfo, _Env) ->
    Event = [{action, <<"connected">>}, {clientid, ClientId}],
    Topic = <<"emqx.stream.devices.connected">>,
    publish_to_nats(Event, Topic).

on_client_disconnected(_ClientInfo = #{clientid := ClientId}, ReasonCode, _ConnInfo, _Env) ->
    Event = [{action, <<"disconnected">>}, {clientid, ClientId}, {reasonCode, ReasonCode}],
    Topic = <<"emqx.stream.devices.disconnected">>,
    publish_to_nats(Event, Topic).

on_client_authenticate(_ClientInfo, Result, _Env) ->
    {ok, Result}.

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message, _Env) ->
    {ok, Payload} = format_payload(Message),
    Topic = <<"emqx.stream.devices.message">>,
    publish_to_nats(Payload, Topic),
    {ok, Message}.

%%--------------------------------------------------------------------
%% Private functions
%%--------------------------------------------------------------------

publish_to_nats(Message, Topic) ->
    emqx_bridge_nats_conn:publish(emqx_json:encode(Message), Topic).

format_payload(Message) ->
    <<T1:64, T2:48, T3:16>> = Message#message.id,
    Payload = [
        {id, T1 + T2 + T3},
        {qos, Message#message.qos},
        {clientid, Message#message.from},
        {topic, Message#message.topic},
        {payload, Message#message.payload}
    ],
    {ok, Payload}.
