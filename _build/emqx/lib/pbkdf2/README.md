pbkdf2
======

A [PBKDF2](http://en.wikipedia.org/wiki/PBKDF2) implementation for [Erlang](http://www.erlang.org) extracted from [Apache CouchDB](http://couchdb.apache.org).

Building
--------

`erlang-pbkdf2` uses [rebar](https://hub.fastgit.org/rebar/rebar) to manage the build process. To build the project, run:

	rebar compile

You can then run the `xref` and `eunit` tests:

	rebar xref eunit

If you want to remove all generated files, run:

	rebar clean

Usage
-----

```erlang
OriginalPassword = <<"password">>.

% Settings
{Salt, Iterations, DerivedLength} = {<<"salt">>, 4096, 20}.

% Hash the original password.
{ok, Key} = pbkdf2:pbkdf2(OriginalPassword, Salt, Iterations, DerivedLength).

% At this point, Key = <<"4b007901b765489abead49d926f721d065a429c1">>.

% Get the password from the user.
EnteredPassword = getpass().

% Ensure that the entered password is the same as the original.
{ok, Key} = pbkdf2:pbkdf2(EnteredPassword, Salt, Iterations, DerivedLength).
```

If you're curious what `getpass/0` would look like, here's a sample implementation:

```erlang
% Get the password from the user.
getpass() ->
	% Store current options for stdio.
	InitialIOOpts = io:getopts(),
	% Disable input character echo.
	ok = io:setopts([{echo, false}]),
	% Prompt the user for a password.
	EnteredPassword = io:get_line("Password: "),
	% Restore original options for stdio.
	ok = io:setopts(InitialIOOpts),
	% Print a newline, since we had local echo disabled above.
	io:format("\n"),
	% Remove trailing newline character, if present.
	case lists:reverse(EnteredPassword) of
		[$\n | Rest] ->
			lists:reverse(Rest);
		_ ->
			EnteredPassword
	end.
```


Cryptographic Software Notice
-----------------------------

This distribution includes cryptographic software. The country in which you
currently reside may have restrictions on the import, possession, use, and/or
re-export to another country, of encryption software. BEFORE using any
encryption software, please check your country's laws, regulations and policies
concerning the import, possession, or use, and re-export of encryption software,
to see if this is permitted. See <http://www.wassenaar.org/> for more
information.

The U.S. Government Department of Commerce, Bureau of Industry and Security
(BIS), has classified this software as Export Commodity Control Number (ECCN)
5D002.C.1, which includes information security software using or performing
cryptographic functions with asymmetric algorithms. The form and manner of this
Apache Software Foundation distribution makes it eligible for export under the
License Exception ENC Technology Software Unrestricted (TSU) exception (see the
BIS Export Administration Regulations, Section 740.13) for both object code and
source code.

The following provides more details on the included cryptographic software:

`erlang-pbkdf2` implements the PBKDF2 key derivation function.
