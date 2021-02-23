%%% -*- erlang -*-
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2016 Ilya Khaprov <ilya.khaprov@publitechs.com>

-module(ssl_verify_pk).

-export([verify_fun/3,
         verify_cert_pk/2]).

-import(ssl_verify_util, [hexstr_to_bin/1]).

-include_lib("public_key/include/public_key.hrl").

-export_type([pk/0,
              user_state/0]).

-type pk() :: {atom(), string() | binary()}.
-type user_state() :: [{check_fingerprint, PL :: pk()}] | [].

%%====================================================================
%% Public API
%%====================================================================

-spec verify_fun(Cert :: #'OTPCertificate'{},
                 Event :: {bad_cert, Reason :: atom() | {revoked, atom()}} |
                          {extension, #'Extension'{}}, InitialUserState :: term()) ->
                    {valid, UserState :: term()} | {valid_peer, UserState :: user_state()} |
                    {fail, Reason :: term()} | {unknown, UserState :: term()}.
verify_fun(Cert, {bad_cert, selfsigned_peer}, UserState) ->
  maybe_verify_cert_pk(Cert, UserState);
verify_fun(_Cert, {bad_cert, unknown_ca}, UserState) ->
  {valid, UserState};
verify_fun(_, {bad_cert, _} = Reason, _UserState) ->
  {fail, Reason};
verify_fun(_, {extension, _}, UserState) ->
  {unknown, UserState};
verify_fun(_, valid, UserState) ->
  {valid, UserState};
verify_fun(Cert, valid_peer, UserState) ->
  maybe_verify_cert_pk(Cert, UserState).

maybe_verify_cert_pk(Cert, UserState) ->
  PK = proplists:get_value(check_pk, UserState),
  case PK of
    undefined -> {valid, UserState};
    _ -> verify_cert_pk(Cert, PK)
  end.

-spec verify_cert_pk(Cert :: #'OTPCertificate'{}, Pk :: pk()) ->
                        {fail, any()} | {valid, _}.
verify_cert_pk(Cert, PK) ->
  {CheckPKAlgorithm, PKStr} = PK,
  case pk_info_to_pk(CheckPKAlgorithm, PKStr) of
    invalid -> {fail, invalid_pk};
    PKB -> verify_cert_pk(Cert, PKB, CheckPKAlgorithm)
  end.

%%====================================================================
%% Private Parts
%%====================================================================

%% plain - just plain bytes
pk_info_to_pk(plain, PK) when is_binary(PK) ->
  PK;
pk_info_to_pk(plain, PK) ->
  hexstr_to_bin(PK);
%% base64 - pk encoded using base64 (same as openssl does)
pk_info_to_pk(base64, PK) when is_binary(PK) ->
  base64:decode(PK);
pk_info_to_pk(base64, PK) when is_list(PK) and (length(PK) rem 4 =:= 0)->
  base64:decode(PK);
%% pk is hashed, do not care about exact algorithm here
pk_info_to_pk(_, PKHash) when is_binary(PKHash) ->
  PKHash;
pk_info_to_pk(_, PKHash) ->
  hexstr_to_bin(PKHash).

verify_cert_pk(Cert, PK, CheckPKAlgorithm) ->
  PublicKey = ssl_verify_fun_cert_helpers:extract_pk(Cert),
  %% pem_entry_encode can't encode ec algorithms
  {'SubjectPublicKeyInfo', Encoded, not_encrypted} =
    public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKey),

  try_match_encoded_pk(CheckPKAlgorithm, Encoded, PK).

try_match_encoded_pk(plain, Encoded, Encoded) ->
  {valid, Encoded};
try_match_encoded_pk(plain, _Encoded, PK) ->
  {fail, PK};
try_match_encoded_pk(base64, Encoded, Encoded) ->
  {valid, Encoded};
try_match_encoded_pk(base64, _Encoded, PK) ->
  {fail, PK};
try_match_encoded_pk(HashAlgorithm, Encoded, PK) ->
  Hash = crypto:hash(HashAlgorithm, Encoded),
  case Hash of
    PK ->
      {valid, PK};
    _ ->
      {fail, pk_no_match}
  end.
