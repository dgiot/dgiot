%%% -*- erlang -*-
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2016 Ilya Khaprov <ilya.khaprov@publitechs.com>

-module(ssl_verify_fingerprint_tests).

-include_lib("eunit/include/eunit.hrl").

google_cert() ->
  load_cert("google.der").

load_cert(Cert) ->
  {ok, Bin} = file:read_file("test/certs/" ++ Cert),
  public_key:pkix_decode_cert(Bin, otp).

verify_google_cert_fingerprint_test () ->
  Cert = google_cert(),
  ?assertEqual({valid, "7FD053FA7F4E6E20DAD4C1262A545782A222A0BC"}, ssl_verify_fingerprint:verify_cert_fingerprint(Cert, {sha, "7FD053FA7F4E6E20DAD4C1262A545782A222A0BC"})),
  ?assertEqual({fail, fingerprint_no_match}, ssl_verify_fingerprint:verify_cert_fingerprint(Cert, {sha, "7FD053FA7F4E6C"})),
  ?assertEqual({fail, invalid_fingerprint}, ssl_verify_fingerprint:verify_cert_fingerprint(Cert, {sha, "7FD053FA7F4E6"})).
