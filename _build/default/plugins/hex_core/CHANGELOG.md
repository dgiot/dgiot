# CHANGELOG

## 0.7.1 (2020-10-12)

* Fix dialyzer warnings on OTP 23

## 0.7.0 (2020-10-20)

* Fix compatibility with OTP 24
* Change `hex_tarball:create/2` error value from `{error, too_big}` to `{tarball, {too_big_compressed | too_big_uncompressed, Size}}`.

## 0.6.10 (2020-08-25)

* Add configuration for max tarball size
* Do not create extraneous "memory" directory when unpacking

## 0.6.9 (2020-05-24)

* Add `hex_api_release:publish/3`

## 0.6.8 (2020-02-04)

* Fix tarball file extraction through symlinks

## 0.6.7 (2020-02-03)

* Fix compatibility with OTP 18 and 17

## 0.6.6 (2020-02-03)

* Do not crash on empty tarballs
* Fix directory traversal vulnerability for symlinks in tarballs

## 0.6.5 (2020-01-14)

* Transition away from `http_uri` when using newer OTP releases
* Ensure `Role` is given as an atom in `hex_api_organization_member:add/3`
* Add `http_headers` to the `hex_core:config()` type
* Add default `http_headers` to `hex_core:default_config/0`
* Fix `hex_registry:decode_and_verify_signed/2` spec
* Fix `hex_tarball:files/0` type

## 0.6.4 (2019-12-27)

* Fix setting level and trasfer options in organization api

## 0.6.3 (2019-12-16)

* Add organization API
* Add level and transfer params to add owner API
* Add `hex_tarball:unpack_docs/2`
* Add `hex_api_auth:test_key/3`

## 0.6.2 (2019-11-11)

* Gracefully handle HTTP errors

## 0.6.1 (2019-07-30)

* Fix bug when publishing with configuration that does not have `repo_organization` key set

## 0.6.0 (2019-07-27)

* **Backwards-incompatible**: Change successful return value of `hex_tarball:create/2` from
  `{ok, {Tarball, Checksum}}` to `{ok, Map}`. Currently, `Map` has `tarball`, `inner_tarball`,
  and `outer_tarball` keys
* Rename existing tarball checksum to inner checksum and deprecate it
* Add tarball outer checksum
* Do not allow comments in `safe_erl_term`
* Forward compatible tarball contents by not failing on unknown files

## 0.5.1 (2019-07-30)

* Fix bug when publishing with configuration that does not have `repo_organization` key set

## 0.5.0 (2019-02-26)

* Fetch package data from proper repository organization and don't verify origin if configured
* **Backwards-incompatible**: Make `hex_api` module private
* **Backwards-incompatible**: `organization` config key has been replaced with `api_organization`
  and `api_repository` keys
* Add `repo_organization` config key
* `vendor.sh`: Add missing `hex_pb_*.erl` files
* `vendor.sh`: Fix replacing `hex_core`

## 0.4.0 (2018-12-30)

* Revert swich from proto2 to proto3
* Verify authenticity of signed payload
* Add `repo_verify_origin` config option

## 0.3.0 (2018-12-14)

* Swich from proto2 to proto3
* Fix `hex_http:body` type
* Fix `hex_http:request` spec

## 0.2.1 (2018-12-13)

* Always return the whole response tuple from requests
* Guess rebar3 as build tool over rebar
* Return compressed tarball from hex_tarball:create_docs/1
* Add package identity to registry records

## 0.2.0 (2018-09-01)

* Accept `{filename(), filename()}` in files list for tarballs
* Support httpc profile through adapter configs
* Update generated protobuf files with OTP 21 compatibility

## 0.1.1 (2018-08-11)

* Use `hex_core` project name in user agent string

## 0.1.0 (2018-08-08)

* First release
