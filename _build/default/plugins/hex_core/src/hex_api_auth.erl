-module(hex_api_auth).
-export([test/2]).

%% @doc
%% Test an auth key against a given domain and resource.
%%
%% Examples:
%%
%% ```
%% 1> Params = #{domain => <<"repository">>, resource => <<"gustafson_motors">>}.
%% 2> hex_api_auth:test_key(hex_core:default_config(), Params).
%% {ok,{204, ..., nil}}
%% '''
%% @end
-spec test(hex_core:config(), map()) -> hex_api:response().
test(Config, #{domain := Domain, resource := Resource}) ->
    URI = ["auth", "?domain=", Domain, "&resource=", Resource],
    hex_api:get(Config, list_to_binary(URI)).
