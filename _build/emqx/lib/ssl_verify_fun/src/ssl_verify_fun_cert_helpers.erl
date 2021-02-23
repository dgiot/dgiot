%%% -*- erlang -*-
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2016 Ilya Khaprov <ilya.khaprov@publitechs.com>

-module(ssl_verify_fun_cert_helpers).

-export([extract_dns_names/1,
         extract_cn/1,
         extract_pk/1]).

-include_lib("public_key/include/public_key.hrl").

%%====================================================================
%% Public API
%%====================================================================

-spec extract_dns_names(Cert :: #'OTPCertificate'{}) -> [] | [string()].
extract_dns_names(Cert)->
  TBSCert = Cert#'OTPCertificate'.tbsCertificate,
  Extensions = extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
  AltSubject = select_extension(?'id-ce-subjectAltName', Extensions),
  case AltSubject of
    undefined ->
      [];
    _ ->
      extract_dns_names_from_alt_names(AltSubject#'Extension'.extnValue, [])
  end.

%% extract cn from subject
-spec extract_cn(Cert :: #'OTPCertificate'{}) -> {error, no_common_name} | {ok, string()} | {error, invalid}.
extract_cn(Cert) ->
  TBSCert = Cert#'OTPCertificate'.tbsCertificate,
  {rdnSequence, List} = TBSCert#'OTPTBSCertificate'.subject,
  extract_cn2(List).

-spec extract_pk(Cert :: #'OTPCertificate'{}) -> {error, no_common_name} | #'SubjectPublicKeyInfo'{}.
extract_pk(Cert) ->
  TBSCert = Cert#'OTPCertificate'.tbsCertificate,
  PublicKeyInfo = TBSCert#'OTPTBSCertificate'.subjectPublicKeyInfo,
  PublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey.

%%====================================================================
%% Private Parts
%%====================================================================

-spec extensions_list([#'Extension'{}] | asn1_NOVALUE) -> [] | [#'Extension'{}].
extensions_list(E) ->
  case E of
    asn1_NOVALUE -> [];
    _ -> E
  end.

-spec select_extension(Id :: term(), [#'Extension'{}]) -> undefined | #'Extension'{}.
select_extension(Id, Extensions) ->
  Matching = [Extension || #'Extension'{extnID = ExtId} = Extension <- Extensions, ExtId =:= Id],
  case Matching of
    [] -> undefined;
    [H|_] -> H
  end.

-spec extract_dns_names_from_alt_names([term()], list()) -> [] | [string()].
extract_dns_names_from_alt_names([ExtValue | Rest], Acc) ->
  Acc1 = case ExtValue of
           {dNSName, DNSName} ->
             [ssl_verify_fun_encodings:decode_dns_name(DNSName) | Acc];
           _ ->
             Acc
         end,
  extract_dns_names_from_alt_names(Rest, Acc1);
extract_dns_names_from_alt_names([], Acc) ->
  Acc.

extract_cn2([[#'AttributeTypeAndValue'{type={2, 5, 4, 3},
                                       value=CN}]|_]) ->
  ssl_verify_fun_encodings:get_string(CN);
extract_cn2([_|Rest]) ->
  extract_cn2(Rest);
extract_cn2([]) ->
  {error, no_common_name}.
