%% This is the default form of error messages for the systems using
%% providers. It is expected that everything that returns an error use
%% this and that they all expose a format_error/2 message that returns
%% an iolist and any changes to state it needs to make on error.

-define(PRV_ERROR(Reason),
        {error, {?MODULE, Reason}}).
