
emqx-reloader
=============

Erlang Module Hot Reloader for Development.

NOTICE: Don't load the plugin in production.

Configure Plugin
----------------

File: etc/emqx_reloader.conf

```
## Interval of hot code reloading.
##
## Value: Duration
##  - h: hour
##  - m: minute
##  - s: second
##
## Examples:
##  - 2h:  2 hours
##  - 30m: 30 minutes
##  - 20s: 20 seconds
##
## Defaut: 60s
reloader.interval = 60s

## Logfile of reloader.
##
## Value: File
reloader.logfile = reloader.log
```

Load the Plugin
---------------

```
./bin/emqx_ctl plugins load emqx_reloader
```

CLI
---

```
./bin/emqx_ctl reload <Module>
```

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.
