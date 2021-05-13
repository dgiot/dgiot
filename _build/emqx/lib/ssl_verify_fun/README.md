# SSL verification for Erlang 

 [![Hex.pm](https://img.shields.io/hexpm/v/ssl_verify_fun.svg?maxAge=2592000)](https://hex.pm/packages/ssl_verify_fun)
 [![Hex.pm](https://img.shields.io/hexpm/dw/ssl_verify_fun.svg?maxAge=2592000)](https://hex.pm/packages/ssl_verify_fun)
 [![Build Status](https://travis-ci.org/deadtrickster/ssl_verify_fun.erl.svg?branch=master)](https://travis-ci.org/deadtrickster/ssl_verify_fun.erl)

* [Fingerprint validation](#certificate-fingerprint-validation--pinning)
* [Public Key validation](#public-key-validation--pinning)
* [Hostname validation](#hostname-validation)

Note: all examples use `{reuse_sessions, false}` to make sure session won't be reused and `ssl:connect` will give you different result when changing fingerprints/hostnames, etc. Perhaps this should be removed in production.

## Resources

- [Certificate and Public Key pinning](https://www.owasp.org/index.php/Certificate_and_Public_Key_Pinning)
- [Pinning Cheatsheet](https://www.owasp.org/index.php/Pinning_Cheat_Sheet)
- [RFC 6125](http://tools.ietf.org/html/rfc6125)

## Certificate fingerprint validation / pinning

[OWASP link](https://www.owasp.org/index.php/Certificate_and_Public_Key_Pinning#Hashing)

```erlang
1> ssl:connect("github.com", 443, [{verify_fun,
				 {fun ssl_verify_fingerprint:verify_fun/3,
				  [{check_fingerprint, {sha, "D79F076110B39293E349AC89845B0380C19E2F8B"} }]}},
				{verify, verify_none},
				{reuse_sessions, false}]).   
{ok,{sslsocket,{gen_tcp,#Port<0.1499>,tls_connection,
                        undefined},
               <0.53.0>}}

2> ssl:connect("google.com", 443, [{verify_fun,
				 {fun ssl_verify_fingerprint:verify_fun/3,
				  [{check_fingerprint, {sha, "D79F076110B39293E349AC89845B0380C19E2F8B"} }]}},
				{verify, verify_none},
				{reuse_sessions, false}]).
=ERROR REPORT==== 10-Mar-2016::16:13:54 ===
SSL: certify: ssl_handshake.erl:1492:Fatal error: handshake failure
{error,{tls_alert,"handshake failure"}}

```

## Public Key validation / pinning

[OWASP link](https://www.owasp.org/index.php/Certificate_and_Public_Key_Pinning#Public_Key)

We can pin public key using its hex or base64 representation as well as fingerprint

Using github.com as example lets extract public key
```
openssl x509 -inform DER  -pubkey -noout -in /tmp/github.com.der

-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA54hc8pZclxgcupjiA/F/
OZGRwm/ZlucoQGTNTKmBEgNsrn/mxhngWmPwbAvUaLP//T79Jc+1WXMpxMiz9PK6
yZRRFuIo0d2bx423NA6hOL2RTtbnfs+y0PFS/YTpQSelTuq+Fuwts5v6aAweNyMc
YD0HBybkkdosFoDccBNzJ92Ac8I5EVDUc3Or/4jSyZwzxu9kdmBlBzeHMvsqdH8S
X9mNahXtXxRpwZnBiUjw36PgN+s9GLWGrafd02T0ux9Yzd5ezkMxukqEAQ7AKIIi
jvaWPAJbK/52XLhIy2vpGNylyni/DQD18bBPT+ZG1uv0QQP9LuY/joO+FKDOTler
4wIDAQAB
-----END PUBLIC KEY-----
```
Openssl prints public key encoded using base64 format. It can be used like that:

```erlang
ssl:connect("github.com", 443, [{verify_fun,
                                {fun ssl_verify_pk:verify_fun/3,
                                 [{check_pk, {base64,
                                              "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA54hc8pZclxgcupjiA/F/"++
                                              "OZGRwm/ZlucoQGTNTKmBEgNsrn/mxhngWmPwbAvUaLP//T79Jc+1WXMpxMiz9PK6"++
                                              "yZRRFuIo0d2bx423NA6hOL2RTtbnfs+y0PFS/YTpQSelTuq+Fuwts5v6aAweNyMc"++
                                              "YD0HBybkkdosFoDccBNzJ92Ac8I5EVDUc3Or/4jSyZwzxu9kdmBlBzeHMvsqdH8S"++
                                              "X9mNahXtXxRpwZnBiUjw36PgN+s9GLWGrafd02T0ux9Yzd5ezkMxukqEAQ7AKIIi"++
                                              "jvaWPAJbK/52XLhIy2vpGNylyni/DQD18bBPT+ZG1uv0QQP9LuY/joO+FKDOTler"++
                                              "4wIDAQAB" } }]}},
                                {verify, verify_none},
                                {reuse_sessions, false}]).      
{ok,{sslsocket,{gen_tcp,#Port<0.2167>,tls_connection,
                        undefined},
               <0.60.0>}}
```

If you don't want to expose public ceritificate or just want to save space,
you can validate public key fingerprint.
![Ubuntu der viewer](http://i.imgur.com/QMolOXV.png)

```erlang
ssl:connect("github.com", 443, [{verify_fun,
                                {fun ssl_verify_pk:verify_fun/3,
                                 [{check_pk, {sha,
                                              "D4EE9D2A6712B3614C272D158B04FCC8CA08A0B6" } }]}},
                                {verify, verify_none},
                                {reuse_sessions, false}]).
{ok,{sslsocket,{gen_tcp,#Port<0.2744>,tls_connection,
	                      undefined},
               <0.73.0>}}
```
As you can see I just copy-pasted Key SHA1 Fingerprint value and removed spaces. It's that easy!

## Hostname validation

Excerpt from RFC (http://tools.ietf.org/html/rfc6125)


```

6.4.3.  Checking of Wildcard Certificates

   1.  The client SHOULD NOT attempt to match a presented identifier in
   which the wildcard character comprises a label other than the
   left-most label (e.g., do not match bar.*.example.net).

   2.  If the wildcard character is the only character of the left-most
   label in the presented identifier, the client SHOULD NOT compare
   against anything but the left-most label of the reference
   identifier (e.g., *.example.com would match foo.example.com but
   not bar.foo.example.com or example.com).

   3.  The client MAY match a presented identifier in which the wildcard
   character is not the only character of the label (e.g.,
   baz*.example.net and *baz.example.net and b*z.example.net would
   be taken to match baz1.example.net and foobaz.example.net and
   buzz.example.net, respectively).  However, the client SHOULD NOT
   attempt to match a presented identifier where the wildcard
   character is embedded within an A-label or U-label [IDNA-DEFS] of
   an internationalized domain name [IDNA-PROTO].

6.4.4.  Checking of Common Names

   As noted, a client MUST NOT seek a match for a reference identifier
   of CN-ID if the presented identifiers include a DNS-ID, SRV-ID,
   URI-ID, or any application-specific identifier types supported by the
   client.

   Therefore, if and only if the presented identifiers do not include a
   DNS-ID, SRV-ID, URI-ID, or any application-specific identifier types
   supported by the client, then the client MAY as a last resort check
   for a string whose form matches that of a fully qualified DNS domain
   name in a Common Name field of the subject field (i.e., a CN-ID).  If
   the client chooses to compare a reference identifier of type CN-ID
   against that string, it MUST follow the comparison rules for the DNS
   domain name portion of an identifier of type DNS-ID, SRV-ID, or
   URI-ID, as described under Section 6.4.1, Section 6.4.2, and
   Section 6.4.3.

```

####Usage###

* With SSL lib or HTTP client you can use provided verify_fun/3, do not forget to add `check_hostname` key to user state:

``` erlang

CACertFile = "..../my-ca.pem".
ssl:connect("tv.eurosport.com", 443, [{verify_fun,
                                       {fun ssl_verify_hostname:verify_fun/3,
                                        [{check_hostname, "tv.eurosport.com"}]}},
                                      {cacertfile, CACertFile },
                                      {server_name_indication, "tv.eurosport.com"},
                                      {reuse_sessions, false},
                                      {verify, verify_peer},
                                      {depth, 99}]).

=ERROR REPORT==== 9-Oct-2014::03:34:41 ===
SSL: certify: ..../ssl_handshake.erl:1403:Fatal error: handshake failure
{error,{tls_alert,"handshake failure"}}

ssl:connect("tv.eurosport.com", 443, [{verify_fun,
                                       {fun ssl_verify_hostname:verify_fun/3, []}},
                                      {cacertfile, CACertFile },
                                      {server_name_indication, "tv.eurosport.com"},
                                      {reuse_sessions, false},
                                      {verify, verify_peer},
                                      {depth, 99}]).

{ok,{sslsocket,{gen_tcp,#Port<0.1565>,tls_connection,
                        undefined},
                        <0.53.0>}}

```

Unfortunately as you can see OTP SSL error reporting not so informative (in fact it ignores everything user-provided verify_fun returns as failure reason (8 October 2014))

``` erlang
path_validation_alert(Reason) ->
    ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE).
```

* With custom verify_fun:
Call `verify_cert_hostname/2` with Certificate and Hostname.
