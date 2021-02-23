%%% -*- erlang -*-
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2016 Ilya Khaprov <ilya.khaprov@publitechs.com>

-module(ssl_verify_pk_tests).

-import(ssl_verify_util, [hexstr_to_bin/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

google_cert() ->
  load_cert("google.der").

load_cert(Cert) ->
  {ok, Bin} = file:read_file("test/certs/" ++ Cert),
  public_key:pkix_decode_cert(Bin, otp).

verify_google_cert_pk_plain_test () ->
  Cert = google_cert(),
  ?assertMatch({valid, _}, ssl_verify_pk:verify_cert_pk(Cert, {plain, "30820122300D06092A864886F70D01010105000382010F003082010A0282010100A2C918063EAB5865F23DD2372441BC10675FE94EBC268022882F8253BDD083B10CE7A25BC44D82772B3931FACC71DB4BD8DC171398B78AFBD692AF880A105931A6F14AC4E454DCB71E8AF3282A88A510660F931D00D4E74C82BEE3F94E6E7F6E165F665ACD24B5D78BA3215581B908CFA9BA0284C67E425B15C8D3D8CC5C9DB745507DA1ECF8205DFC8DF265B641CE59758F8FB08AD915BF7613E0CBAB82A2A515E89A477649109D42CB093309A98E5AD66615EA4863B9A079BDFB7F609CDB25327D1D89C45882CB928C2F0185CFAD74C6FEE1FF3537338E3EBE1BC0D8088BF07FA420FECB8D068F51A6BB90A9CC28807B52FE0D936BB08E28FE4AE54A3368D70203010001"})),
  ?assertMatch({fail, _}, ssl_verify_pk:verify_cert_pk(Cert, {plain, "qwe"})),
  ?assertMatch({valid, _}, ssl_verify_pk:verify_cert_pk(Cert, {plain, hexstr_to_bin("30820122300D06092A864886F70D01010105000382010F003082010A0282010100A2C918063EAB5865F23DD2372441BC10675FE94EBC268022882F8253BDD083B10CE7A25BC44D82772B3931FACC71DB4BD8DC171398B78AFBD692AF880A105931A6F14AC4E454DCB71E8AF3282A88A510660F931D00D4E74C82BEE3F94E6E7F6E165F665ACD24B5D78BA3215581B908CFA9BA0284C67E425B15C8D3D8CC5C9DB745507DA1ECF8205DFC8DF265B641CE59758F8FB08AD915BF7613E0CBAB82A2A515E89A477649109D42CB093309A98E5AD66615EA4863B9A079BDFB7F609CDB25327D1D89C45882CB928C2F0185CFAD74C6FEE1FF3537338E3EBE1BC0D8088BF07FA420FECB8D068F51A6BB90A9CC28807B52FE0D936BB08E28FE4AE54A3368D70203010001")})),
  ?assertMatch({fail, _}, ssl_verify_pk:verify_cert_pk(Cert, {plain, <<"qwe">>})).

verify_google_cert_pk_base64_test () ->
  Cert = google_cert(),
  ?assertMatch({valid, _}, ssl_verify_pk:verify_cert_pk(Cert, {base64, "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAoskYBj6rWGXyPdI3JEG8EGdf6U68JoAiiC+CU73Qg7EM56JbxE2Cdys5MfrMcdtL2NwXE5i3ivvWkq+IChBZMabxSsTkVNy3HorzKCqIpRBmD5MdANTnTIK+4/lObn9uFl9mWs0ktdeLoyFVgbkIz6m6AoTGfkJbFcjT2MxcnbdFUH2h7PggXfyN8mW2Qc5ZdY+PsIrZFb92E+DLq4KipRXomkd2SRCdQssJMwmpjlrWZhXqSGO5oHm9+39gnNslMn0dicRYgsuSjC8Bhc+tdMb+4f81NzOOPr4bwNgIi/B/pCD+y40Gj1Gmu5CpzCiAe1L+DZNrsI4o/krlSjNo1wIDAQAB"})),
  ?assertMatch({valid, _}, ssl_verify_pk:verify_cert_pk(Cert, {base64, <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAoskYBj6rWGXyPdI3JEG8EGdf6U68JoAiiC+CU73Qg7EM56JbxE2Cdys5MfrMcdtL2NwXE5i3ivvWkq+IChBZMabxSsTkVNy3HorzKCqIpRBmD5MdANTnTIK+4/lObn9uFl9mWs0ktdeLoyFVgbkIz6m6AoTGfkJbFcjT2MxcnbdFUH2h7PggXfyN8mW2Qc5ZdY+PsIrZFb92E+DLq4KipRXomkd2SRCdQssJMwmpjlrWZhXqSGO5oHm9+39gnNslMn0dicRYgsuSjC8Bhc+tdMb+4f81NzOOPr4bwNgIi/B/pCD+y40Gj1Gmu5CpzCiAe1L+DZNrsI4o/krlSjNo1wIDAQAB">>})),
  ?assertMatch({fail, _}, ssl_verify_pk:verify_cert_pk(Cert, {base64, "MIIB"})),
  ?assertMatch({fail, _}, ssl_verify_pk:verify_cert_pk(Cert, {base64, <<"MIIB">>})).

verify_google_cert_pk_sha_test () ->
  Cert = google_cert(),
  ?assertMatch({valid, _}, ssl_verify_pk:verify_cert_pk(Cert, {sha, "6B8525451DC66F0CD490621F6B03FB2E7DFA493C"})),
  ?assertMatch({valid, _}, ssl_verify_pk:verify_cert_pk(Cert, {sha, hexstr_to_bin("6B8525451DC66F0CD490621F6B03FB2E7DFA493C")})),
  ?assertMatch({fail, _}, ssl_verify_pk:verify_cert_pk(Cert, {sha, "6B8525451DC66F0CD490621F6B03FB2E7DFA49"})),
  ?assertMatch({fail, _}, ssl_verify_pk:verify_cert_pk(Cert, {sha, hexstr_to_bin("6B8525451DC66F0CD490621F6B03FB2E7DFA49")})).
