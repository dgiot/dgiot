%% @doc
%% hex_core entrypoint module.
%%
%% ### Config
%%
%% Most functions in the hex_core API takes a configuration. The configuration sets things
%% like HTTP client to use, and API and repository URL. Some of these configuration options
%% will likely be static for your application and some may change depending on the function
%% you call.
%%
%% ##### Options
%%
%% * `api_key' - Authentication key used when accessing the HTTP API.
%% * `api_organization' - Name of the organization endpoint in the API, this should
%%   for example be set when accessing key for a specific organization.
%% * `api_repository' - Name of the repository endpoint in the API, this should
%%   for example be set when accessing packages from a specific repository.
%% * `api_url' - URL to the HTTP API (default: `https://hex.pm/api').
%% * `http_adapter' - Callback module used for HTTP requests, see [`hex_http'](hex_http.html)
%%   (default: `hex_http_httpc').
%% * `http_etag' - Sets the `if-none-match' HTTP header with the given value to do a
%%   conditional HTTP request.
%% * `http_adapter_config' - Configuration to pass to the HTTP adapter.
%% * `http_user_agent_fragment' - Will be appended to the `user-agent` HTTP header (default: `(httpc)').
%% * `repo_key' - Authentication key used when accessing the repository.
%% * `repo_name' - Name of the repository, used for verifying the repository signature
%%   authenticity (default: `hexpm').
%% * `repo_public_key' - Public key used to verify the repository signature
%%   (defaults to hexpm public key `https://hex.pm/docs/public_keys').
%% * `repo_url' - URL to the repository (default: `https://repo.hex.pm').
%% * `repo_organization' - Name of the organization repository, appends `/repos/:name'
%%    to the repository URL and overrides the `repo_name' option.
%% * `repo_verify' - If `true' will verify the repository signature (default: `true').
%% * `repo_verify_origin' - If `true' will verify the repository signature origin,
%%   requires protobuf messages as of hex_core v0.4.0 (default: `true').

-module(hex_core).
-export([default_config/0]).

-export_type([config/0]).

%% https://hex.pm/docs/public_keys
-define(HEXPM_PUBLIC_KEY, <<"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
0wIDAQAB
-----END PUBLIC KEY-----">>).


-type config() :: #{
    api_key => binary() | undefined,
    api_organization => binary() | undefined,
    api_repository => binary() | undefined,
    api_url => binary(),
    http_adapter => module(),
    http_etag => binary() | undefined,
    http_adapter_config => map(),
    http_headers => map(),
    http_user_agent_fragment => binary(),
    repo_key => binary() | undefined,
    repo_name => binary(),
    repo_public_key => binary(),
    repo_url => binary(),
    repo_organization => binary() | undefined,
    repo_verify => boolean(),
    repo_verify_origin => boolean()
}.

-spec default_config() -> config().
default_config() ->
    #{
        api_key => undefined,
        api_organization => undefined,
        api_repository => undefined,
        api_url => <<"https://hex.pm/api">>,
        http_adapter => hex_http_httpc,
        http_adapter_config => #{profile => default},
        http_etag => undefined,
        http_user_agent_fragment => <<"(httpc)">>,
        repo_key => undefined,
        repo_name => <<"hexpm">>,
        repo_public_key => ?HEXPM_PUBLIC_KEY,
        repo_url => <<"https://repo.hex.pm">>,
        repo_organization => undefined,
        repo_verify => true,
        repo_verify_origin => true,
        http_headers => #{}
    }.
