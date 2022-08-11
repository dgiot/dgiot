#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

EMQX_APP='apps/'
EMQX_DASHBOARD_PATH='lib-ce/emqx_dashboard/priv'
DGIOT_DASHBOARD_PATH='apps/dgiot_api/priv'
FILESEVER="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"

get_emqx_dashboard() {
  if [ -d "$EMQX_DASHBOARD_PATH/www" ]; then
    echo "emqx_dashboard exist"
  else
    wget "$FILESEVER"/emqx_dashboard.tar.gz
    tar xvf emqx_dashboard.tar.gz
    mv www "$EMQX_DASHBOARD_PATH/"
    rm emqx_dashboard.tar.gz -rf
    chmod 777 ./scripts/*
  fi
}

get_dgiot_dashboard() {
  if [ -d "$DGIOT_DASHBOARD_PATH/www" ]; then
    echo "dgiot_dashboard exist"
  else
    wget "$FILESEVER"/www.tar.gz
    tar xvf www.tar.gz
    mv www "$DGIOT_DASHBOARD_PATH/"
    rm www.tar.gz -rf
    chmod 777 ./scripts/*
  fi
}

get_apps() {
  cd "$EMQX_APP"

  if [ ! -d "emqx_rule_engine/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-rule-engine.git" emqx_rule_engine
  fi

  if [ ! -d "emqx_bridge_mqtt/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-bridge-mqtt.git" emqx_bridge_mqtt
  fi

  if [ ! -d "emqx_coap/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-coap.git" emqx_coap
  fi

  if [ ! -d "emqx_stomp/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-stomp.git" emqx_stomp
  fi

  if [ ! -d "emqx_lwm2m/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-lwm2m.git" emqx_lwm2m
  fi

  if [ ! -d "emqx_exhook/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-exhook.git" emqx_exhook
  fi

  if [ ! -d "emqx_exproto/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-exproto.git" emqx_exproto
  fi

  if [ ! -d "emqx_auth_mnesia/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-auth-mnesia.git" emqx_auth_mnesia
  fi

  if [ ! -d "emqx_lua_hook/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-lua-hook.git" emqx_lua_hook
  fi

  if [ ! -d "emqx_management/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-management.git" emqx_management
  fi

  if [ ! -d "emqx_prometheus/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-prometheus.git" emqx_prometheus
  fi

  if [ ! -d "emqx_psk_file/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-psk-file.git" emqx_psk_file
  fi

  if [ ! -d "emqx_recon/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-recon.git" emqx_recon
  fi

  if [ ! -d "emqx_retainer/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-retainer.git" emqx_retainer
  fi

  if [ ! -d "emqx_sasl/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-sasl.git" emqx_sasl
  fi

  if [ ! -d "emqx_sn/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-sn.git" emqx_sn
  fi

  if [ ! -d "emqx_web_hook/" ]; then
    git clone -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-web-hook.git" emqx_web_hook
  fi

  cd ..
}

get_apps
get_dgiot_dashboard
get_emqx_dashboard
