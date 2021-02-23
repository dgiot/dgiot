rebar3 Hex Providers
=========================

Providers for interacting with the Erlang package manager [hex.pm](https://hex.pm/).


Usage
------

Add to your global rebar3 config in `~/.config/rebar3/rebar.config`:

```erlang
{plugins, [rebar3_hex]}.
```

Usage
--------

## Authenticating User

If you already have a user for [hex.pm](https://hex.pm) run:


```shell
$ rebar3 hex user auth
```

Note that this will ask for your hex.pm username and password, as well as a password for encrypting your api token that has write permissions to the repository. When publishing a package you will have to give this password to decrypt the token in order to publish.

## Private Organizations

[Private organizations]() are treated as repositories that have a parent. The parent is found as the first part of a repository's name, separated from the organization by a `:`. So for an organization `rebar3` on the main repository `hexpm` the name must be `hexpm:rebar3`.

`~/.config/rebar3/rebar.config`

```erlang
{hex, [{repos, [
		#{name => <<"hexpm:rebar3">>}
	  ]}]}.
```

## Read-Only Repo Key for CI

If you have a private organization or other private repository it is recommended that you use a repo specific auth token for reading from the repository in CI. To generate a token:

```shell
$ rebar3 hex repo auth <repo> generate
Generated key: abc123
```

Then in CI use whatever method is available for setting an environment variable to the token and add this call at the beginning of your CI runs to add the token to your rebar3 hex tokens:

```shell
$ rebar3 hex repo auth <repo> --key $REPO_KEY
```

## Publishing Packages

Two functions are available for publishing applications to hex as packages. The first, `hex publish`, simply packages up what you have as is:

``` shell
$ rebar3 hex publish
```

Note that it will display the details of what it is publishing (what files, the version, dependencies) and ask if it should continue, so be sure to read the output carefully and make sure it is publishing what you expected.

Another task, `hex cut` is available to provide some additional functionality around versioning and git tags:

``` shell
$ rebar3 hex cut
```

This command will ask how to increment (major, minor, patch) the version number based on the last version and can create a git tag and push that tag upstream.

## Publishing Docs

``` shell
$ rebar3 hex docs
```

## Adding and Removing Owners of Packages

Owners can be added and removed for packages you are an owner of with the `hex owner` command:

``` shell
$ rebar3 hex owner [add | remove] <package> <email>
```
