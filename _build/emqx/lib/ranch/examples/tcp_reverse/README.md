Ranch TCP reverse example
=========================

This example uses a `gen_statem` to handle a protocol to revese input.
See `reverse_protocol.erl` for the implementation. Documentation about
this topic can be found in the guide:

  http://ninenines.eu/docs/en/ranch/HEAD/guide/protocols/#using_gen_statem

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/tcp_reverse_example/bin/tcp_reverse_example console
```

Then start a telnet session to port 5555:

``` bash
$ telnet localhost 5555
```

Type in a few words and see them reversed! Amazing!

Be aware that there is a timeout of 5 seconds without receiving
data before the example server disconnects your session.
