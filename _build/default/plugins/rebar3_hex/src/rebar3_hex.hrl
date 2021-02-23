-define(DEFAULT_FILES, ["src", "c_src", "include", "rebar.config.script"
                       ,"priv", "rebar.config", "rebar.lock"
                       ,"README*", "readme*"
                       ,"LICENSE*", "license*"
                       ,"NOTICE"]).
-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).
-define(DEFAULT_HEX_REPO, <<"hexpm">>).
