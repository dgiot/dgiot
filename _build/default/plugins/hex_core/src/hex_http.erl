-module(hex_http).
-export([request/5]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("hex_core.hrl").

-type method() :: get | post | put | patch | delete.
-type status() :: non_neg_integer().
-export_type([status/0]).
-type headers() :: #{binary() => binary()}.
-export_type([headers/0]).
-type body() :: {ContentType :: binary(), Body :: binary()} | undefined.
-type adapter_config() :: map().

-callback request(method(), URI :: binary(), headers(), body(), adapter_config()) ->
    {ok, status(), headers(), binary()} |
    {error, term()}.

-spec request(hex_core:config(), method(), URI :: binary(), headers(), body()) ->
    {ok, {status(), headers(), binary()}} | {error, term()}.
request(Config, Method, URI, Headers, Body) when is_binary(URI) and is_map(Headers) ->
    {Adapter, AdapterConfig} = case maps:get(http_adapter, Config, {hex_http_httpc, #{}}) of
        {Adapter0, AdapterConfig0} ->
            {Adapter0, AdapterConfig0};
        %% TODO: remove in v0.9
        Adapter0 when is_atom(Adapter0) ->
            AdapterConfig0 = maps:get(http_adapter_config, Config, #{}),
            io:format("[hex_http] setting #{http_adapter => Module, http_adapter_config => Map} "
                      "is deprecated in favour of #{http_adapter => {Module, Map}}~n"),
            {Adapter0, AdapterConfig0}
    end,
    UserAgentFragment = maps:get(http_user_agent_fragment, Config),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
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
