#!/bin/sh
## EMQ docker image start script
# Huang Rui <vowstar@gmail.com>
# EMQ X Team <support@emqx.io>

## Shell setting
if [[ ! -z "$DEBUG" ]]; then
    set -ex
else
    set -e
fi

## Local IP address setting

LOCAL_IP=$(hostname -i |grep -E -oh '((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])'|head -n 1)

## EMQ Base settings and plugins setting
# Base settings in /opt/emqx/etc/emqx.conf
# Plugin settings in /opt/emqx/etc/plugins

_EMQX_HOME="/opt/emqx"

if [[ -z "$PLATFORM_ETC_DIR" ]]; then
    export PLATFORM_ETC_DIR="$_EMQX_HOME/etc"
fi

if [[ -z "$PLATFORM_LOG_DIR" ]]; then
    export PLATFORM_LOG_DIR="$_EMQX_HOME/log"
fi

if [[ -z "$EMQX_NAME" ]]; then
    export EMQX_NAME="$(hostname)"
fi

if [[ -z "$EMQX_HOST" ]]; then
    if [[ "$EMQX_CLUSTER__K8S__ADDRESS_TYPE" == "dns" ]] && [[ ! -z "$EMQX_CLUSTER__K8S__NAMESPACE" ]];then
        EMQX_CLUSTER__K8S__SUFFIX=${EMQX_CLUSTER__K8S__SUFFIX:-"pod.cluster.local"}
        DNSAddress="${LOCAL_IP//./-}.${EMQX_CLUSTER__K8S__NAMESPACE}.${EMQX_CLUSTER__K8S__SUFFIX}"
        export EMQX_HOST="$DNSAddress"
    elif [[ "$EMQX_CLUSTER__K8S__ADDRESS_TYPE" == "hostname" ]] && [[ ! -z "$EMQX_CLUSTER__K8S__NAMESPACE" ]]; then
        EMQX_CLUSTER__K8S__SUFFIX=${EMQX_CLUSTER__K8S__SUFFIX:-"svc.cluster.local"}
        HostAddress=$(sed -n "/^${LOCAL_IP}/"p /etc/hosts | grep -e "$(hostname).*.${EMQX_CLUSTER__K8S__NAMESPACE}.${EMQX_CLUSTER__K8S__SUFFIX}" -o)
        export EMQX_HOST="$HostAddress"
    else
        export EMQX_HOST="$LOCAL_IP"
    fi
fi

if [[ -z "$EMQX_WAIT_TIME" ]]; then
    export EMQX_WAIT_TIME=5
fi

if [[ -z "$EMQX_NODE_NAME" ]]; then
    export EMQX_NODE_NAME="$EMQX_NAME@$EMQX_HOST"
fi

# Set hosts to prevent cluster mode failed

# unset EMQX_NAME
# unset EMQX_HOST

if [[ -z "$EMQX_NODE__PROCESS_LIMIT" ]]; then
    export EMQX_NODE__PROCESS_LIMIT=2097152
fi

if [[ -z "$EMQX_NODE__MAX_PORTS" ]]; then
    export EMQX_NODE__MAX_PORTS=1048576
fi

if [[ -z "$EMQX_NODE__MAX_ETS_TABLES" ]]; then
    export EMQX_NODE__MAX_ETS_TABLES=2097152
fi

if [[ -z "$EMQX__LOG_CONSOLE" ]]; then
    export EMQX__LOG_CONSOLE="console"
fi

if [[ -z "$EMQX_LISTENER__TCP__EXTERNAL__ACCEPTORS" ]]; then
    export EMQX_LISTENER__TCP__EXTERNAL__ACCEPTORS=64
fi

if [[ -z "$EMQX_LISTENER__TCP__EXTERNAL__MAX_CLIENTS" ]]; then
    export EMQX_LISTENER__TCP__EXTERNAL__MAX_CLIENTS=1000000
fi

if [[ -z "$EMQX_LISTENER__SSL__EXTERNAL__ACCEPTORS" ]]; then
    export EMQX_LISTENER__SSL__EXTERNAL__ACCEPTORS=32
fi

if [[ -z "$EMQX_LISTENER__SSL__EXTERNAL__MAX_CLIENTS" ]]; then
    export EMQX_LISTENER__SSL__EXTERNAL__MAX_CLIENTS=500000
fi

if [[ -z "$EMQX_LISTENER__WS__EXTERNAL__ACCEPTORS" ]]; then
    export EMQX_LISTENER__WS__EXTERNAL__ACCEPTORS=16
fi

if [[ -z "$EMQX_LISTENER__WS__EXTERNAL__MAX_CLIENTS" ]]; then
    export EMQX_LISTENER__WS__EXTERNAL__MAX_CLIENTS=250000
fi

# Fix issue #42 - export env EMQX_DASHBOARD__DEFAULT_USER__PASSWORD to configure
# 'dashboard.default_user.password' in etc/plugins/emqx_dashboard.conf
if [[ ! -z "$EMQX_ADMIN_PASSWORD" ]]; then
    export EMQX_DASHBOARD__DEFAULT_USER__PASSWORD=$EMQX_ADMIN_PASSWORD
fi

# Catch all EMQX_ prefix environment variable and match it in configure file
CONFIG="${_EMQX_HOME}/etc/emqx.conf"
CONFIG_PLUGINS="${_EMQX_HOME}/etc/plugins"
for VAR in $(env)
do
    # Config normal keys such like node.name = emqx@127.0.0.1
    if [[ ! -z "$(echo $VAR | grep -E '^EMQX_')" ]]; then
        VAR_NAME=$(echo "$VAR" | sed -r "s/EMQX_([^=]*)=.*/\1/g" | tr '[:upper:]' '[:lower:]' | sed -r "s/__/\./g")
        VAR_FULL_NAME=$(echo "$VAR" | sed -r "s/([^=]*)=.*/\1/g")
        # Config in emq.conf
        if [[ ! -z "$(cat $CONFIG |grep -E "^(^|^#*|^#*\s*)$VAR_NAME")" ]]; then
            echo "$VAR_NAME=$(eval echo \$$VAR_FULL_NAME)"
            if [[ -z "$(eval echo \$$VAR_FULL_NAME)" ]]; then
                echo "$(sed -r "s/(^\s*)($VAR_NAME\s*=\s*.*)/#\2/g" $CONFIG)" > $CONFIG
            else
                echo "$(sed -r "s/(^#*\s*)($VAR_NAME)\s*=\s*(.*)/\2 = $(eval echo \$$VAR_FULL_NAME|sed -e 's/\//\\\//g')/g" $CONFIG)" > $CONFIG
            fi
        fi
        # Config in plugins/*
        for CONFIG_PLUGINS_FILE in $(ls $CONFIG_PLUGINS); do
            if [[ ! -z "$(cat $CONFIG_PLUGINS/$CONFIG_PLUGINS_FILE |grep -E "^(^|^#*|^#*\s*)$VAR_NAME")" ]]; then
                echo "$VAR_NAME=$(eval echo \$$VAR_FULL_NAME)"
                if [[ -z "$(eval echo \$$VAR_FULL_NAME)" ]]; then
                    echo "$(sed -r "s/(^\s*)($VAR_NAME\s*=\s*.*)/#\2/g" $CONFIG_PLUGINS/$CONFIG_PLUGINS_FILE)" > $CONFIG_PLUGINS/$CONFIG_PLUGINS_FILE
                else
                    echo "$(sed -r "s/(^#*\s*)($VAR_NAME)\s*=\s*(.*)/\2 = $(eval echo \$$VAR_FULL_NAME|sed -e 's/\//\\\//g')/g" $CONFIG_PLUGINS/$CONFIG_PLUGINS_FILE)" > $CONFIG_PLUGINS/$CONFIG_PLUGINS_FILE
                fi
            fi 
        done
    fi
    # Config template such like {{ platform_etc_dir }}
    if [[ ! -z "$(echo $VAR | grep -E '^PLATFORM_')" ]]; then
        VAR_NAME=$(echo "$VAR" | sed -r "s/([^=]*)=.*/\1/g"| tr '[:upper:]' '[:lower:]')
        VAR_FULL_NAME=$(echo "$VAR" | sed -r "s/([^=]*)=.*/\1/g")
        echo "$(sed -r "s@\{\{\s*$VAR_NAME\s*\}\}@$(eval echo \$$VAR_FULL_NAME|sed -e 's/\//\\\//g')@g" $CONFIG)" > $CONFIG
    fi
done

## EMQ Plugin load settings
# Plugins loaded by default

if [[ ! -z "$EMQX_LOADED_PLUGINS" ]]; then
    echo "EMQX_LOADED_PLUGINS=$EMQX_LOADED_PLUGINS"
    # First, remove special char at header
    # Next, replace special char to ".\n" to fit emq loaded_plugins format
    echo $(echo "$EMQX_LOADED_PLUGINS."|sed -e "s/^[^A-Za-z0-9_]\{1,\}//g"|sed -e "s/[^A-Za-z0-9_]\{1,\}/\. /g")|tr ' ' '\n' > /opt/emqx/data/loaded_plugins
fi

exec "$@"