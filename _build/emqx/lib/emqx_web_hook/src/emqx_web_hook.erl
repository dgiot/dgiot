%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_web_hook).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-define(APP, emqx_web_hook).

-logger_header("[WebHook]").

%% APIs
-export([ register_metrics/0
        , load/0
        , unload/0
        ]).

%% Hooks callback
-export([ on_client_connect/3
        , on_client_connack/4
        ]).
-export([ on_client_connected/3
        , on_client_disconnected/4
        ]).
-export([ on_client_subscribe/4
        , on_client_unsubscribe/4
        ]).
-export([ on_session_subscribed/4
        , on_session_unsubscribed/4
        , on_session_terminated/4
        ]).
-export([ on_message_publish/2
        , on_message_delivered/3
        , on_message_acked/3
        ]).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ['web_hook.client_connect',
                                           'web_hook.client_connack',
                                           'web_hook.client_connected',
                                           'web_hook.client_disconnected',
                                           'web_hook.client_subscribe',
                                           'web_hook.client_unsubscribe',
                                           'web_hook.session_subscribed',
                                           'web_hook.session_unsubscribed',
                                           'web_hook.session_terminated',
                                           'web_hook.message_publish',
                                           'web_hook.message_delivered',
                                           'web_hook.message_acked']).

load() ->
    lists:foreach(
      fun({Hook, Fun, Filter}) ->
        load_(Hook, binary_to_atom(Fun, utf8), {Filter})
      end, parse_rule(application:get_env(?APP, rules, []))).

unload() ->
    lists:foreach(
      fun({Hook, Fun, _Filter}) ->
          unload_(Hook, binary_to_atom(Fun, utf8))
      end, parse_rule(application:get_env(?APP, rules, []))).

%%--------------------------------------------------------------------
%% Client connect
%%--------------------------------------------------------------------

on_client_connect(ConnInfo = #{clientid := ClientId, username := Username, peername := {Peerhost, _}}, _ConnProp, _Env) ->
    emqx_metrics:inc('web_hook.client_connect'),
    Params = [{action, client_connect},
              {clientid, ClientId},
              {username, Username},
              {ipaddress, iolist_to_binary(ntoa(Peerhost))},
              {keepalive, maps:get(keepalive, ConnInfo)},
              {proto_ver, maps:get(proto_ver, ConnInfo)}],
    send_http_request(Params),
    ok;
on_client_connect(#{}, _ConnProp, _Env) ->
    ok.

%%--------------------------------------------------------------------
%% Client connack
%%--------------------------------------------------------------------

on_client_connack(ConnInfo = #{clientid := ClientId, username := Username, peername := {Peerhost, _}}, Rc, _AckProp, _Env) ->
    emqx_metrics:inc('web_hook.client_connack'),
    Params = [{action, client_connack},
              {clientid, ClientId},
              {username, Username},
              {ipaddress, iolist_to_binary(ntoa(Peerhost))},
              {keepalive, maps:get(keepalive, ConnInfo)},
              {proto_ver, maps:get(proto_ver, ConnInfo)},
              {conn_ack, Rc}],
    send_http_request(Params),
    ok;
on_client_connack(#{}, _Rc, _AckProp, _Env) ->
    ok.

%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------

on_client_connected(#{clientid := ClientId, username := Username, peerhost := Peerhost}, ConnInfo, _Env) ->
    emqx_metrics:inc('web_hook.client_connected'),
    Params = [{action, client_connected},
              {clientid, ClientId},
              {username, Username},
              {ipaddress, iolist_to_binary(ntoa(Peerhost))},
              {keepalive, maps:get(keepalive, ConnInfo)},
              {proto_ver, maps:get(proto_ver, ConnInfo)},
              {connected_at, maps:get(connected_at, ConnInfo)}],
    send_http_request(Params),
    ok;

on_client_connected(#{}, _ConnInfo, _Env) ->
    ok.

%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------

on_client_disconnected(ClientInfo, {shutdown, Reason}, ConnInfo, Env) when is_atom(Reason) ->
    on_client_disconnected(ClientInfo, Reason, ConnInfo, Env);
on_client_disconnected(#{clientid := ClientId, username := Username}, Reason, _ConnInfo, _Env) ->
    emqx_metrics:inc('web_hook.client_disconnected'),
    Params = [{action, client_disconnected},
              {clientid, ClientId},
              {username, Username},
              {reason, printable(Reason)}],
    send_http_request(Params),
    ok.

printable(Term) when is_atom(Term); is_binary(Term) ->
    Term;
printable(Term) when is_tuple(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

%%--------------------------------------------------------------------
%% Client subscribe
%%--------------------------------------------------------------------

on_client_subscribe(#{clientid := ClientId, username := Username}, _Properties, TopicTable, {Filter}) ->
    lists:foreach(fun({Topic, Opts}) ->
      with_filter(
        fun() ->
          emqx_metrics:inc('web_hook.client_subscribe'),
          Params = [{action, client_subscribe},
                    {clientid, ClientId},
                    {username, Username},
                    {topic, Topic},
                    {opts, Opts}],
          send_http_request(Params)
        end, Topic, Filter)
    end, TopicTable).

%%--------------------------------------------------------------------
%% Client unsubscribe
%%--------------------------------------------------------------------

on_client_unsubscribe(#{clientid := ClientId, username := Username}, _Properties, TopicTable, {Filter}) ->
    lists:foreach(fun({Topic, Opts}) ->
      with_filter(
        fun() ->
          emqx_metrics:inc('web_hook.client_unsubscribe'),
          Params = [{action, client_unsubscribe},
                    {clientid, ClientId},
                    {username, Username},
                    {topic, Topic},
                    {opts, Opts}],
          send_http_request(Params)
        end, Topic, Filter)
    end, TopicTable).

%%--------------------------------------------------------------------
%% Session subscribed
%%--------------------------------------------------------------------

on_session_subscribed(#{clientid := ClientId, username := Username}, Topic, Opts, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('web_hook.session_subscribed'),
        Params = [{action, session_subscribed},
                  {clientid, ClientId},
                  {username, Username},
                  {topic, Topic},
                  {opts, Opts}],
        send_http_request(Params)
      end, Topic, Filter).

%%--------------------------------------------------------------------
%% Session unsubscribed
%%--------------------------------------------------------------------

on_session_unsubscribed(#{clientid := ClientId, username := Username}, Topic, _Opts, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('web_hook.session_unsubscribed'),
        Params = [{action, session_unsubscribed},
                  {clientid, ClientId},
                  {username, Username},
                  {topic, Topic}],
        send_http_request(Params)
      end, Topic, Filter).

%%--------------------------------------------------------------------
%% Session terminated
%%--------------------------------------------------------------------

on_session_terminated(Info, {shutdown, Reason}, SessInfo, Env) when is_atom(Reason) ->
    on_session_terminated(Info, Reason, SessInfo, Env);
on_session_terminated(#{clientid := ClientId, username := Username}, Reason, _SessInfo, _Env) when is_atom(Reason) ->
    emqx_metrics:inc('web_hook.session_terminated'),
    Params = [{action, session_terminated},
              {clientid, ClientId},
              {username, Username},
              {reason, Reason}],
    send_http_request(Params),
    ok;
on_session_terminated(#{}, Reason, _SessInfo, _Env) ->
    ?LOG(error, "Session terminated, cannot encode the reason: ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Message publish
%%--------------------------------------------------------------------

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};
on_message_publish(Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('web_hook.message_publish'),
        {FromClientId, FromUsername} = format_from(Message),
        Params = [{action, message_publish},
                  {from_client_id, FromClientId},
                  {from_username, FromUsername},
                  {topic, Message#message.topic},
                  {qos, Message#message.qos},
                  {retain, Retain},
                  {payload, encode_payload(Message#message.payload)},
                  {ts, Message#message.timestamp}],
        send_http_request(Params),
        {ok, Message}
      end, Message, Topic, Filter).

%%--------------------------------------------------------------------
%% Message deliver
%%--------------------------------------------------------------------

on_message_delivered(#{clientid := ClientId, username := Username}, Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
  with_filter(
    fun() ->
      emqx_metrics:inc('web_hook.message_delivered'),
      {FromClientId, FromUsername} = format_from(Message),
      Params = [{action, message_delivered},
                {clientid, ClientId},
                {username, Username},
                {from_client_id, FromClientId},
                {from_username, FromUsername},
                {topic, Message#message.topic},
                {qos, Message#message.qos},
                {retain, Retain},
                {payload, encode_payload(Message#message.payload)},
                {ts, Message#message.timestamp}],
      send_http_request(Params)
    end, Topic, Filter).

%%--------------------------------------------------------------------
%% Message acked
%%--------------------------------------------------------------------

on_message_acked(#{clientid := ClientId}, Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
    with_filter(
      fun() ->
        emqx_metrics:inc('web_hook.message_acked'),
        {FromClientId, FromUsername} = format_from(Message),
        Params = [{action, message_acked},
                  {clientid, ClientId},
                  {from_client_id, FromClientId},
                  {from_username, FromUsername},
                  {topic, Message#message.topic},
                  {qos, Message#message.qos},
                  {retain, Retain},
                  {payload, encode_payload(Message#message.payload)},
                  {ts, Message#message.timestamp}],
        send_http_request(Params)
      end, Topic, Filter).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

send_http_request(Params) ->
    Params1 = jsx:encode(Params),
    Url = application:get_env(?APP, url, "http://127.0.0.1"),
    ?LOG(debug, "Url:~p, params:~s", [Url, Params1]),
    case request_(post, {Url, [], "application/json", Params1}, [{timeout, 5000}], [], 0) of
        {ok, _} -> ok;
        {error, Reason} ->
            ?LOG(error, "HTTP request error: ~p", [Reason]), ok %% TODO: return ok?
    end.

request_(Method, Req, HTTPOpts, Opts, Times) ->
    %% Resend request, when TCP closed by remotely
    case httpc:request(Method, Req, HTTPOpts, Opts) of
        {error, socket_closed_remotely} when Times < 3 ->
            timer:sleep(trunc(math:pow(10, Times))),
            request_(Method, Req, HTTPOpts, Opts, Times+1);
        Other -> Other
    end.

parse_rule(Rules) ->
    parse_rule(Rules, []).
parse_rule([], Acc) ->
    lists:reverse(Acc);
parse_rule([{Rule, Conf} | Rules], Acc) ->
    Params = jsx:decode(iolist_to_binary(Conf)),
    Action = proplists:get_value(<<"action">>, Params),
    Filter = proplists:get_value(<<"topic">>, Params),
    parse_rule(Rules, [{list_to_atom(Rule), Action, Filter} | Acc]).

with_filter(Fun, _, undefined) ->
    Fun(), ok;
with_filter(Fun, Topic, Filter) ->
    case emqx_topic:match(Topic, Filter) of
        true  -> Fun(), ok;
        false -> ok
    end.

with_filter(Fun, _, _, undefined) ->
    Fun();
with_filter(Fun, Msg, Topic, Filter) ->
    case emqx_topic:match(Topic, Filter) of
        true  -> Fun();
        false -> {ok, Msg}
    end.

format_from(#message{from = ClientId, headers = #{username := Username}}) ->
    {a2b(ClientId), a2b(Username)};
format_from(#message{from = ClientId, headers = _HeadersNoUsername}) ->
    {a2b(ClientId), <<"undefined">>}.

encode_payload(Payload) ->
    encode_payload(Payload, application:get_env(?APP, encode_payload, undefined)).

encode_payload(Payload, base62) -> emqx_base62:encode(Payload);
encode_payload(Payload, base64) -> base64:encode(Payload);
encode_payload(Payload, _) -> Payload.

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.

load_(Hook, Fun, Params) ->
    case Hook of
        'client.connect'      -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'client.connack'      -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'client.connected'    -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'client.disconnected' -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'client.subscribe'    -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'client.unsubscribe'  -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'session.subscribed'  -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'session.unsubscribed'-> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'session.terminated'  -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'message.publish'     -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'message.acked'       -> emqx:hook(Hook, {?MODULE, Fun, [Params]});
        'message.delivered'   -> emqx:hook(Hook, {?MODULE, Fun, [Params]})
    end.

unload_(Hook, Fun) ->
    case Hook of
        'client.connect'      -> emqx:unhook(Hook, {?MODULE, Fun});
        'client.connack'      -> emqx:unhook(Hook, {?MODULE, Fun});
        'client.connected'    -> emqx:unhook(Hook, {?MODULE, Fun});
        'client.disconnected' -> emqx:unhook(Hook, {?MODULE, Fun});
        'client.subscribe'    -> emqx:unhook(Hook, {?MODULE, Fun});
        'client.unsubscribe'  -> emqx:unhook(Hook, {?MODULE, Fun});
        'session.subscribed'  -> emqx:unhook(Hook, {?MODULE, Fun});
        'session.unsubscribed'-> emqx:unhook(Hook, {?MODULE, Fun});
        'session.terminated'  -> emqx:unhook(Hook, {?MODULE, Fun});
        'message.publish'     -> emqx:unhook(Hook, {?MODULE, Fun});
        'message.acked'       -> emqx:unhook(Hook, {?MODULE, Fun});
        'message.delivered'   -> emqx:unhook(Hook, {?MODULE, Fun})
    end.

ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
    inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) ->
    inet_parse:ntoa(IP).
