%%% -*- erlang -*-
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2016 Ilya Khaprov <ilya.khaprov@publitechs.com>

-module(ssl_verify_hostname_tests).

-include_lib("eunit/include/eunit.hrl").

verify_hostname_success_test_ () ->
  %% presented identifier, reference identifier, validation and parsing result
  Tests = [
           {"www.example.com", "WWW.eXamPle.CoM", false}, %% case insensitive match
           {"www.example.com.", "www.example.com", false}, %% ignore trailing dots (prevenet *.com. matches)
           {"www.example.com", "www.example.com.", false},
           {"*.example.com", "www.example.com", {[], ".example.com", true}},         %% always matching wildcards
           {"b*z.example.com", "buzz.example.com", {"b", "z.example.com", false}},
           {"*baz.example.com", "foobaz.example.com", {[], "baz.example.com", false}},
           {"baz*.example.com", "baz1.example.com", {"baz", ".example.com", false}}
          ],
  [{lists:append(join(" : ", [I, R])),
    fun() ->
            ?assertMatch(V, ssl_verify_hostname:parse_and_validate_wildcard_identifier(I, R)),
            ?assert(ssl_verify_hostname:try_match_hostname(I, R))
    end} || {I, R, V} <- Tests].


verify_hostname_fail_test_ () ->
  %% presented identifier, reference identifier
  Tests = [
           {"*.com", "eXamPle.CoM"},
           {".com.", "example.com."},
           {"*.www.example.com", "www.example.com."},
           {"foo.*.example.com", "foo.bar.example.com."},
           {"xn--*.example.com", "xn-foobar.example.com"},
           {"*fooxn--bar.example.com", "bazfooxn--bar.example.com"},
           {"*.akamaized.net", "tv.eurosport.com"},
           {"a*c.example.com", "abcd.example.com"},
           {"*baz.example.com", "foobuzz.example.com"}
          ],
  [{lists:append(join(" : ", [I, R])),
    fun() -> ?assertNot(ssl_verify_hostname:try_match_hostname(I, R)) end} || {I, R} <- Tests].


%% Certs generated via:
%% actual cert content dumped via:
%% ssl:connect("google.co.uk", 443, [{verify_fun, {fun(C, E, State) -> io:format(user, "C: ~p~n", [C]), {valid, state} end, state}}])
%% then we write them to der via:
%% C = {..}.
%% B = public_key:pkix_encode('OTPCertificate', C, 'otp').
%% file:write_file("google_teletex.der", B).


google_cert() ->
  load_cert("google.der").

load_cert(Cert) ->
  {ok, Bin} = file:read_file("test/certs/" ++ Cert),
  public_key:pkix_decode_cert(Bin, otp).

google_cert_dns_wildcard() ->
  load_cert("google_wildcard.der").


google_cert_without_dns() ->
  load_cert("google_nodns.der").


google_cert_printable_string() ->
  load_cert("google_printable.der").


google_cert_teletex_string() ->
  load_cert("google_teletex.der").

google_cert_bmp_string() ->
  load_cert("google_bmp.der").

google_cert_universal_string() ->
  load_cert("google_universal.der").

verify_google_cert_test () ->
  ?assertEqual({valid, "google.co.uk"}, ssl_verify_hostname:verify_fun(google_cert(), valid_peer, [{check_hostname, "google.co.uk"}])).

verify_google_cert_dns_wildcard_test () ->
  ?assertEqual({valid, "www.google.co.uk"}, ssl_verify_hostname:verify_fun(google_cert_dns_wildcard(), valid_peer, [{check_hostname, "www.google.co.uk"}])).

verify_google_cert_without_dns_test () ->
  ?assertEqual({valid, "www.google.co.uk"}, ssl_verify_hostname:verify_fun(google_cert_without_dns(), valid_peer, [{check_hostname, "www.google.co.uk"}])).

verify_google_cert_printable_string_test() ->
  ?assertEqual({valid, "www.google.co.uk"}, ssl_verify_hostname:verify_fun(google_cert_printable_string(), valid_peer, [{check_hostname, "www.google.co.uk"}])).

verify_google_cert_teletex_string_test() ->
  ?assertEqual({valid, "www.google.co.uk"}, ssl_verify_hostname:verify_fun(google_cert_teletex_string(), valid_peer, [{check_hostname, "www.google.co.uk"}])).

verify_google_cert_bmp_string_test() ->
  ?assertEqual({valid, "google.co.uk"}, ssl_verify_hostname:verify_fun(google_cert_bmp_string(), valid_peer, [{check_hostname, "google.co.uk"}])).

verify_google_cert_universal_string_test() ->
  ?assertEqual({valid, "google.co.uk"}, ssl_verify_hostname:verify_fun(google_cert_universal_string(), valid_peer, [{check_hostname, "google.co.uk"}])).

%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-spec join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: term().

join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep, H|join_prepend(Sep, T)].
