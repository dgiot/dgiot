%% Types
-type master_name() :: atom().
-type master_host() :: string().
-type master_port() :: integer().

%% Sentinel constants
-define(SENTINEL_PORT, 26379).

% Sentinel errors
-define(SENTINEL_UNREACHABLE, sentinel_unreachable).
-define(MASTER_UNKNOWN, master_unknown).
-define(MASTER_UNREACHABLE, master_unreachable).
