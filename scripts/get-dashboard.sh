#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

EMQX_APP='apps/'
EMQX_DASHBOARD_PATH='lib-ce/emqx_dashboard/priv'
DGIOT_DASHBOARD_PATH='apps/dgiot_api/priv'
FILESEVER="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"



get_dashboard(){
  if [ -d "$EMQX_DASHBOARD_PATH/www" ]; then
      echo "emqx_dashboard exist"
  else
    wget ${FILESEVER}/www.tar.gz
    tar xvf www.tar.gz
    mv www "$EMQX_DASHBOARD_PATH/"
    rm www.tar.gz -rf
    chmod 777 ./scripts/*
  fi

  if [ -d "$DGIOT_DASHBOARD_PATH/www" ]; then
      echo "dgiot_dashboard exist"
  else
      git clone -b gh-pages https://gitee.com/dgiiot/dgiot-dashboard.git dist
      rm dist/.git/ -rf
      mv dist  "$DGIOT_DASHBOARD_PATH/www"

      ## dgiot_file
      rm dgiot_file.tar.gz -rf
      wget ${FILESEVER}/dgiot_file.tar.gz
      rm dgiot_file/ -rf
      tar xvf dgiot_file.tar.gz
      rm dgiot_file.tar.gz -rf
      mv  dgiot_file "$DGIOT_DASHBOARD_PATH/www/"

      ## swagger文件
      rm dgiot_swagger.tar.gz -rf
      wget ${FILESEVER}/dgiot_swagger.tar.gz
      rm dgiot_swagger/ -rf
      tar xvf dgiot_swagger.tar.gz
      rm dgiot_swagger.tar.gz -rf
      mv dgiot_swagger "$DGIOT_DASHBOARD_PATH/www/"

      ## dgiot_dashboard静态文件
      rm dgiot_dashboard.tar.gz -rf
      wget ${FILESEVER}/dgiot_dashboard.tar.gz
      rm dgiot_dashboard/ -rf
      tar xvf dgiot_dashboard.tar.gz
      rm dgiot_dashboard.tar.gz -rf
      mv  dgiot_dashboard "$DGIOT_DASHBOARD_PATH/www/"
  fi
}

get_apps() {
    cd ${EMQX_APP}
    if [ ! -d "emqx_rule_engine/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-rule-engine.git"  emqx_rule_engine
    fi
    if [ ! -d "emqx_bridge_mqtt/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-bridge-mqtt.git"  emqx_bridge_mqtt
    fi

    if [ ! -d "emqx_coap/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-coap.git"  emqx_coap
    fi

    if [ ! -d "emqx_stomp/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-stomp.git"  emqx_stomp
    fi

    if [ ! -d "emqx_lwm2m/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-lwm2m.git"  emqx_lwm2m
    fi

    if [ ! -d "emqx_exhook/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-exhook.git"  emqx_exhook
    fi

     if [ ! -d "emqx_exproto/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-exproto.git"  emqx_exproto
    fi

    if [ ! -d "emqx_auth_mnesia/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-auth-mnesia.git"  emqx_auth_mnesia
    fi

    if [ ! -d "emqx_lua_hook/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-lua-hook.git"  emqx_lua_hook
    fi

    if [ ! -d "emqx_management/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-management.git"  emqx_management
    fi

    if [ ! -d "emqx_prometheus/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-prometheus.git"  emqx_prometheus
    fi

    if [ ! -d "emqx_psk_file/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-psk-file.git"  emqx_psk_file
    fi

    if [ ! -d "emqx_recon/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-recon.git"  emqx_recon
    fi

    if [ ! -d "emqx_retainer/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-retainer.git"  emqx_retainer
    fi

    if [ ! -d "emqx_sasl/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-sasl.git"  emqx_sasl
    fi

    if [ ! -d "emqx_sasl/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-sasl.git"  emqx_sasl
    fi

    if [ ! -d "emqx_sn/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-sn.git"  emqx_sn
    fi

    if [ ! -d "emqx_web_hook/" ]; then
      git clone  -b "v4.3.10" "https://gitee.com/fastdgiot/emqx-web-hook.git"  emqx_web_hook
    fi

    cd ..
}


 get_apps
 get_dashboard
