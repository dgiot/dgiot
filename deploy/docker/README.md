# EMQ X Docker

TODO: ...

*EMQ* (Erlang MQTT Broker) is a distributed, massively scalable, highly extensible MQTT messaging broker written in Erlang/OTP.

Current docker image size: 47 MB

### Build emqx from source

1. Update docker configuration to enable docker manifest command and prepare qemu to build images other then x86_64
    ```bash
    make prepare
    ```
2. Build Docker image
    ```bash
    make build
    ```
3. Test the docker image with paho
    ```bash
    make test 
    ```
4. Tag Docker image
    ```bash
    make tag 
    ```
5. Save the docker image as a zip file
   ```bash
    make save 
    ``` 
6. Push Docker image
   ```bash
    docker login
    make push
    make manifest-list
    docker logout
    ```
7. Clean up the compiled image
    ```bash
    make clear
    ```

### Get emqx from the docker hub

You can pull the image on the [docker hub](https://hub.docker.com/r/emqx/emqx).

```bash
docker pull emqx/emqx:latest
```

### Run emqx

Execute some command under this docker image

``docker run -d -v `pwd`:$(somewhere) emqx/emqx:$(tag) $(somecommand)``

For example

``docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx:latest``

The emqx broker runs as linux user `emqx` in the docker container.

### Configuration

Use the environment variable to configure the EMQ X docker container.

The environment variables which with ``EMQX_`` prefix are mapped to configuration file, ``.`` get replaced by ``__``.

Example:

```bash
EMQX_LISTENER__SSL__EXTERNAL__ACCEPTORS <--> listener.ssl.external.acceptors
EMQX_MQTT__MAX_PACKET_SIZE              <--> mqtt.max_packet_size
```

Also the environment variables which with ``PLATFORM_`` prefix are mapped to template string in configuration file.

```bash
PLATFORM_ETC_DIR                   <--> {{ platform_etc_dir }}
```

Non mapped environment variables:

```bash
EMQX_NAME
EMQX_HOST
```

These environment variables will ignore for configuration file.

#### EMQ X Configuration

> NOTE: All EMQ X Configuration in [etc/emqx.conf](https://github.com/emqx/emqx/blob/emqx30/etc/emqx.conf) could config by environment. The following list is just an example, not a complete configuration.

| Options                    | Default            | Mapped                    | Description                           |
| ---------------------------| ------------------ | ------------------------- | ------------------------------------- |
| EMQX_NAME                   | container name     | none                      | emqx node short name                   |
| EMQX_HOST                   | container IP       | none                      | emqx node host, IP or FQDN             |
| EMQX_WAIT_TIME              | 5                  | none                      | wait time in sec before timeout       |
| PLATFORM_ETC_DIR            | /opt/emqx/etc      | {{ platform_etc_dir }}    | The etc directory                     |
| PLATFORM_LOG_DIR            | /opt/emqx/log      | {{ platform_log_dir }}    | The log directory                     |
| EMQX_NODE__NAME             | EMQX_NAME@EMQX_HOST| node.name                 | Erlang node name, name@ipaddress/host |
| EMQX_NODE__COOKIE           | emqx_dist_cookie    | node.cookie               | cookie for cluster                    |
| EMQX_LOG__CONSOLE           | console            | log.console               | log console output method             |
| EMQX_ALLOW_ANONYMOUS        | true               | allow_anonymous           | allow mqtt anonymous login            |
| EMQX_LISTENER__TCP__EXTERNAL| 1883               | listener.tcp.external     | MQTT TCP port                         |
| EMQX_LISTENER__SSL__EXTERNAL| 8883               | listener.ssl.external     | MQTT TCP TLS/SSL port                 |
| EMQX_LISTENER__WS__EXTERNAL | 8083               | listener.ws.external      | HTTP and WebSocket port               |
| EMQX_LISTENER__WSS__EXTERNAL| 8084               | listener.wss.external     | HTTPS and WSS port                    |
| EMQX_LISTENER__API__MGMT    | 8080               | listener.api.mgmt         | MGMT API  port                        |
| EMQX_MQTT__MAX_PACKET_SIZE  | 64KB               | mqtt.max_packet_size      | Max Packet Size Allowed               |

The list is incomplete and may changed with [etc/emqx.conf](https://github.com/emqx/emqx/blob/emqx30/etc/emqx.conf) and plugin configuration files. But the mapping rule is similar.

If set ``EMQX_NAME`` and ``EMQX_HOST``, and unset ``EMQX_NODE__NAME``, ``EMQX_NODE__NAME=$EMQX_NAME@$EMQX_HOST``.

For example, set mqtt tcp port to 1883

``docker run -d --name emqx -e EMQX_LISTENER__TCP__EXTERNAL=1883 -p 18083:18083 -p 1883:1883 emqx/emqx:latest``

#### EMQ Loaded Plugins Configuration

| Oprtions                 | Default            | Description                           |
| ------------------------ | ------------------ | ------------------------------------- |
| EMQX_LOADED_PLUGINS       | see content below  | default plugins emqx loaded            |

Default environment variable ``EMQX_LOADED_PLUGINS``, including

- ``emqx_recon``
- ``emqx_retainer``
- ``emqx_management``
- ``emqx_dashboard``

```bash
# The default EMQX_LOADED_PLUGINS env
EMQX_LOADED_PLUGINS="emqx_recon,emqx_retainer,emqx_management,emqx_dashboard"
```
**When you need to customize the loaded plugin, ``emqx_management`` must be loaded in the first place.**

For example, load ``emqx_auth_redis`` plugin, set it into ``EMQX_LOADED_PLUGINS`` and use any separator to separates it.

You can use comma, space or other separator that you want.

All the plugin you defined in env ``EMQX_LOADED_PLUGINS`` will be loaded.

```bash
EMQX_LOADED_PLUGINS="emqx_management,emqx_auth_redis,emqx_recon,emqx_retainer,emqx_dashboard"
EMQX_LOADED_PLUGINS="emqx_management emqx_auth_redis emqx_recon emqx_retainer emqx_dashboard"
EMQX_LOADED_PLUGINS="emqx_management | emqx_auth_redis | emqx_recon | emqx_retainer | emqx_dashboard"
```

#### EMQ X Plugins Configuration

The environment variables which with ``EMQX_`` prefix are mapped to all emqx plugins' configuration file, ``.`` get replaced by ``__``.

Example:

```bash
EMQX_AUTH__REDIS__SERVER   <--> auth.redis.server
EMQX_AUTH__REDIS__PASSWORD <--> auth.redis.password
```

Don't worry about where to find the configuration file of emqx plugins, this docker image will find and config them automatically using some magic.

All plugin of emqx project could config in this way, following the environment variables mapping rule above.

Assume you are using redis auth plugin, for example:

```bash
#EMQX_AUTH__REDIS__SERVER="redis.at.yourserver"
#EMQX_AUTH__REDIS__PASSWORD="password_for_redis"

docker run -d --name emqx -p 18083:18083 -p 1883:1883 -p 4369:4369 \
    -e EMQX_LISTENER__TCP__EXTERNAL=1883 \
    -e EMQX_LOADED_PLUGINS="emqx_auth_redis,emqx_recon,emqx_retainer,emqx_management,emqx_dashboard" \
    -e EMQX_AUTH__REDIS__SERVER="your.redis.server:6379" \
    -e EMQX_AUTH__REDIS__PASSWORD="password_for_redis" \
    -e EMQX_AUTH__REDIS__PASSWORD_HASH=plain \
    emqx/emqx:latest

```

### Cluster

You can specify a initial cluster and join.

> Note: You must publsh port 4369, 5369 and range of port 6000-6999 for EMQ X Cluster.

For example, using 6000-6100 for cluster.

```bash

docker run -d --name emqx -p 18083:18083 -p 1883:1883 -p 4369:4369 -p 6000-6100:6000-6100 \
    -e EMQX_NAME="emqx" \
    -e EMQX_HOST="t.emqx.io" \
    -e EMQX_LISTENER__TCP__EXTERNAL=1883 \
    -e EMQX_JOIN_CLUSTER="emqx@t.emqx.io" \
    emqx/emqx:latest

```

### Kernel Tuning

Under linux host machine, the easiest way is [tuning host machine's kernel](http://emqttd-docs.readthedocs.io/en/latest/tune.html).

If you want tune linux kernel by docker, you must ensure your docker is latest version (>=1.12).

```bash

docker run -d --name emqx -p 18083:18083 -p 1883:1883 -p 4369:4369 \
    --sysctl fs.file-max=2097152 \
    --sysctl fs.nr_open=2097152 \
    --sysctl net.core.somaxconn=32768 \
    --sysctl net.ipv4.tcp_max_syn_backlog=16384 \
    --sysctl net.core.netdev_max_backlog=16384 \
    --sysctl net.ipv4.ip_local_port_range=1000 65535 \
    --sysctl net.core.rmem_default=262144 \
    --sysctl net.core.wmem_default=262144 \
    --sysctl net.core.rmem_max=16777216 \
    --sysctl net.core.wmem_max=16777216 \
    --sysctl net.core.optmem_max=16777216 \
    --sysctl net.ipv4.tcp_rmem=1024 4096 16777216 \
    --sysctl net.ipv4.tcp_wmem=1024 4096 16777216 \
    --sysctl net.ipv4.tcp_max_tw_buckets=1048576 \
    --sysctl net.ipv4.tcp_fin_timeout=15 \
    emqx/emqx:latest

```

> REMEMBER: DO NOT RUN EMQ X DOCKER PRIVILEGED OR MOUNT SYSTEM PROC IN CONTAINER TO TUNE LINUX KERNEL, IT IS UNSAFE.

### Thanks

+ [@je-al](https://github.com/emqx/emqx-docker/issues/2)
+ [@RaymondMouthaan](https://github.com/emqx/emqx-docker/pull/91)
