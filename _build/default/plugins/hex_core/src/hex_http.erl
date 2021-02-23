-module(hex_http).
-export([request/5]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("hex_core.hrl").

-type method() :: get | post | put | patch | delete.
-type status() :: non_neg_integer().
-type headers() :: #{binary() => binary()}.
-type body() :: {ContentType :: binary(), Body :: binary()} | undefined.
-type adapter_config() :: map().

-callback request(method(), URI :: binary(), headers(), body(), adapter_config()) ->
    {ok, status(), headers(), binary()} |
    {error, term()}.

-spec request(hex_core:config(), method(), URI :: binary(), headers(), body()) ->
    {ok, {status(), headers(), binary()}} | {error, term()}.
request(Config, Method, URI, Headers, Body) when is_binary(URI) and is_map(Headers) ->
    Adapter = maps:get(http_adapter, Config),
    UserAgentFragment = maps:get(http_user_agent_fragment, Config),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    AdapterConfig = maps:get(http_adapter_config, Config, #{}),
    Adapter:request(Method, URI, Headers2, Body, AdapterConfig).

user_agent(UserAgentFragment) ->
    OTPRelease = erlang:system_info(otp_release),
    ERTSVersion = erlang:system_info(version),
    OTPString = " (OTP/" ++ OTPRelease ++ ") (erts/" ++ ERTSVersion ++ ")",
    iolist_to_binary(["hex_core/", ?HEX_CORE_VERSION, " ", UserAgentFragment, OTPString]).

%%====================================================================
%% Internal functions
%%====================================================================

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
