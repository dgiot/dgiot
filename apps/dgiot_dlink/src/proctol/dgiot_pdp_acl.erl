%%--------------------------------------------------------------------
%% P2 ABAC - PDP-backed ACL for the dgiot/{site}/{gateway}/{device}/{point}/data
%% doctrine topic namespace (iotStudio absorption program, OPA-shaped contract).
%%
%% Decisions come from an attribute policy decision point over loopback HTTP
%% (default http://127.0.0.1:8383/mqtt/acl). Response contract (same shape
%% as EMQX emqx_auth_http):
%%   "allow"  -> {stop, allow}
%%   "deny"   -> {stop, deny}
%%   "ignore" -> ok    (no opinion; rest of the ACL chain decides)
%%
%% Scope: ONLY topics prefixed dgiot/ are decided here; everything else
%% (including the $dg/ namespace owned by dgiot_mqtt_acl) returns ok.
%%
%% Fail mode: open (ok) by default for availability; switch to fail-closed
%% with dgiot:set_env(dgiot_dlink, pdp_fail, closed).
%% Policy changes are hot-reloaded by the PDP - the broker never restarts.
%%--------------------------------------------------------------------
-module(dgiot_pdp_acl).

-export([check/3]).

check(PubSub, Topic, ClientInfo) ->
    case Topic of
        <<"dgiot/", _/binary>> ->
            ask(PubSub, Topic, ClientInfo);
        _ ->
            ok
    end.

ask(PubSub, Topic, ClientInfo) ->
    Username = maps:get(username, ClientInfo, <<>>),
    ClientId = maps:get(clientid, ClientInfo, <<>>),
    Action = atom_to_binary(PubSub, utf8),
    Url = dgiot:get_env(dgiot_dlink, pdp_url, <<"http://127.0.0.1:8383/mqtt/acl">>),
    Body = jsx:encode(#{<<"username">> => Username,
                        <<"clientid">> => ClientId,
                        <<"topic">> => Topic,
                        <<"action">> => Action}),
    Req = {binary_to_list(Url), [], "application/json", Body},
    HttpOpts = [{timeout, 2000}, {connect_timeout, 1000}],
    case catch httpc:request(post, Req, HttpOpts, []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            case string:strip(binary_to_list(iolist_to_binary(RespBody)), both) of
                "allow" -> {stop, allow};
                "deny" -> {stop, deny};
                _ -> ok
            end;
        _ ->
            case dgiot:get_env(dgiot_dlink, pdp_fail, open) of
                closed -> {stop, deny};
                _ -> ok
            end
    end.
