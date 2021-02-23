# Cuttlefish

Cuttlefish is a library for Erlang applications that wish to walk the
fine line between Erlang `app.config`s and a sysctl-like syntax.
The name is a pun on the pronunciation of 'sysctl' and jokes are
better explained.

## Riak Disclaimer

While this readme and test suite is Riak-heavy, the fact is that this
library can be used with any Erlang application that wants a more
universally accessible configuration syntax. Still, I built this for
Riak, and it's nice to have a concrete example to work with.

## The Vision

Currently, Riak's `app.config` is **the** definitive place for
configuring Riak. It's not odd for Erlang applications to be
configured this way, but it is a struggle for non-Erlang programmers
and automated deployment tools to manipulate these files. On the other
hand, the `app.config` is a useful construct for Erlang programmers,
and it is pretty coupled to OTP applications.

Cuttlefish's goal is to put a layer of abstraction on top of the
`app.config` that is easier to work with outside of the Erlang world.
It will allow Erlang programmers to write a schema for their
application's configuration file, which is independent of the
applications included in the project. The schema is one of the more
important parts of Cuttlefish, so we'll go into more detail on it
below, but it is written in Erlang and defines how the non-Erlang
configuration file works.

From this schema, you can generate a default `.conf` file for your
application. This will be the file that is packaged with your
application as the default configuration.

The schema is also used to generate an `app.config` that will be used
to start your application. Using the schema alone will generate all
the proper defaults. Your users can make changes to the `.conf` file
and those changes will overwrite the schema's defaults.

You an also have an `advanced.config` which looks like the old
`app.config` for anything that no schema mapping is created for.

What does this look like for an application like Riak?

Well, the authors of Riak maintain a schema for Riak's config. It
defines all sorts of things we'll get into later. When we build Riak,
Cuttlefish generates a `riak.conf` file that contains the default
shipping configuration of Riak. When a script to start Riak is run, a
Cuttlefish escript is spun up, reads the `riak.conf` file and combines
that with the Schema to generate an `app.config`. The script then
exits, and a new Erlang VM (destined to run Riak) is started with that
generated `app.config`. Down the line somewhere, you may be
troubleshooting some part of Riak, and the support organization at
Basho may need you to manipulate a configuration setting that is not
exposed by the schema because it is so infrequently used. In that
case, we can set that setting directly in an `advanced.config` which
sits in the same directory as `riak.conf`.

I hope that gives you a good idea about how this works at a high
level.

## What's it look like to Erlang Developers?

You can learn more about the technical implementation of schemas at:
https://github.com/basho/cuttlefish/wiki/Cuttlefish-for-Erlang-Developers

## What's it look like to users?

Riak uses the semantic of `$conf_dir/app.config` for configuration.
We're going to replace that with a file called `riak.conf`, with a
syntax that looks like this:

```ini
ring_size = 32
anti_entropy = debug
log.error.file = /var/log/error.log
log.console.file = /var/log/console.log
log.syslog = on
```

More information for users here:
https://github.com/basho/cuttlefish/wiki/Cuttlefish-for-Application-Users

## What's it look like to application packagers?

* [node_package](https://github.com/basho/cuttlefish/wiki/Cuttlefish-for-node_package-users)
* [non node_package](https://github.com/basho/cuttlefish/wiki/Cuttlefish-for-non-node_package-users)


## Current Status

Cuttlefish is ready for production deployments.
