#!/usr/bin/env escript
main(_Options) ->
    OtpRelease = otp_release(erlang:system_info(otp_release)),
    ok = io:format("~s", [OtpRelease]).

%% Copied from https://github.com/rebar/rebar3/blob/master/src/rebar_utils.erl
otp_release([$R,N|_]=Rel) when is_integer(N) ->
    Rel;
otp_release(Rel) ->
    File = filename:join([code:root_dir(), "releases", Rel, "OTP_VERSION"]),
    case file:read_file(File) of
        {error, _} ->
            Rel;
        {ok, Vsn} ->
            Size = byte_size(Vsn),
            case binary:part(Vsn, {Size, -3}) of
                <<"**\n">> ->
                    binary:bin_to_list(Vsn, {0, Size - 3});
                _ ->
                    binary:bin_to_list(Vsn, {0, Size - 1})
            end
    end.