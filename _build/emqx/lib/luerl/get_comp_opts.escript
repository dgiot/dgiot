#! /usr/bin/env escript
%% -*- mode: erlang; indent-tabs-mode: nil -*-
%% Define a number of compiler options. We first work out the current
%% Erlang version and from the we can define the various options.

%% Bloody useful.
-define(IF(Test,True,False), case Test of true -> True; false -> False end).

%% Define the makefile variables HAS_MAPS and HAS_FULL_KEYS depending
%% on whether this version of erlang has maps (17) and general map
%% keys (18), or NEW_CORE_REC for new core definition of records (19).

-define(HAS_MAPS_OPT, "-DHAS_MAPS=true").
-define(FULL_KEYS_OPT, "-DHAS_FULL_KEYS=true").
-define(NEW_REC_OPT, "-DNEW_REC_CORE=true").
-define(NEW_RAND_OPT, "-DNEW_RAND=true").

main(_) ->
    Version = otp_release(),
    CompOpts = comp_opts(Version),
    file:write_file("comp_opts.mk", "COMP_OPTS = " ++ CompOpts ++ "\n").

comp_opts(Version) ->
    Copts0 = "-DERLANG_VERSION=\\\"" ++ Version ++ "\\\"",
    Copts1 = ?IF(Version >= "17", Copts0 ++ " " ++ ?HAS_MAPS_OPT, Copts0),
    Copts2 = ?IF(Version >= "18", Copts1 ++ " " ++ ?FULL_KEYS_OPT, Copts1),
    Copts3 = ?IF(Version >= "19",
                 Copts2 ++ append_copts([?NEW_REC_OPT,?NEW_RAND_OPT]),
                 Copts2),
    Copts3.

append_copts([Copt|Copts]) ->
    " " ++ Copt ++ append_copts(Copts);
append_copts([]) -> [].

%% Get the major release number.
%% We have stolen the idea for this code from rebar3.

otp_release() ->
    case erlang:system_info(otp_release) of
        [$R,N1|Rest] when is_integer(N1) ->
            %% If OTP <= R16, take the digits.
            [N1|Rest];
        Rel ->
            %% If OTP >= 17.x, erlang:system_info(otp_release) returns
            %% just the major version number.
            File = filename:join([code:root_dir(),"releases",Rel,"OTP_VERSION"]),
            case file:read_file(File) of
                {error, _} -> Rel;
                {ok, Vsn} ->
                    Size = byte_size(Vsn),
                    %% The shortest vsn string consists of at least
                    %% two digits followed by "\n". Therefore, it's
                    %% safe to assume Size >= 3.
                    case binary:part(Vsn, {Size, -3}) of
                        <<"**\n">> ->
                            binary:bin_to_list(Vsn, {0, Size - 3});
                        _ ->
                            binary:bin_to_list(Vsn, {0, Size - 1})
                    end
            end
    end.
