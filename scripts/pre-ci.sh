#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

EMQX_APP='apps/'
EMQX_LIBCE_PATH='lib-ce/'
DGIOT_DASHBOARD_PATH='apps/dgiot_api/priv'
get_lib_ce(){
   cd ${EMQX_LIBCE_PATH}
   if [ ! -d "emqx_dashboard/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-dashboard.git"  emqx_dashboard
   fi

   if [ ! -d "emqx_modules/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-modules.git"  emqx_modules
   fi

   if [ ! -d "emqx_telemetry/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-telemetry.git"  emqx_telemetry
   fi

   cd ..
}

get_apps() {
    cd ${EMQX_APP}

    if [ ! -d "emqx_plugin_libs/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx_plugin_libs.git"  emqx_plugin_libs
    fi

    if [ ! -d "emqx_rule_engine/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-rule-engine.git"  emqx_rule_engine
    fi
    if [ ! -d "emqx_bridge_mqtt/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-bridge-mqtt.git"  emqx_bridge_mqtt
    fi

    if [ ! -d "emqx_exhook/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-exhook.git"  emqx_exhook
    fi

    if [ ! -d "emqx_auth_mnesia/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-auth-mnesia.git"  emqx_auth_mnesia
    fi

    if [ ! -d "emqx_management/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-management.git"  emqx_management
    fi

    if [ ! -d "emqx_prometheus/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-prometheus.git"  emqx_prometheus
    fi

    if [ ! -d "emqx_psk_file/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-psk-file.git"  emqx_psk_file
    fi

    if [ ! -d "emqx_recon/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-recon.git"  emqx_recon
    fi

    if [ ! -d "emqx_retainer/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-retainer.git"  emqx_retainer
    fi

    if [ ! -d "emqx_sasl/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-sasl.git"  emqx_sasl
    fi

    if [ ! -d "emqx_sn/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-sn.git"  emqx_sn
    fi

    cd ..
}

 echo "ci"
 get_apps
 get_lib_ce

