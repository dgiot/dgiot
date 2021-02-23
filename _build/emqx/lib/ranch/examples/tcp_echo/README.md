Ranch TCP echo example
======================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/tcp_echo_example/bin/tcp_echo_example console
```

Then start a telnet session to port 5555:

``` bash
$ telnet localhost 5555
```

Type in a few words and see them echoed back.

Be aware that there is a timeout of 5 seconds without receiving
data before the example server disconnects your session.
