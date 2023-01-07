#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

PKG_VSN="${PKG_VSN:-$(./pkg-vsn.sh)}"
case "${PKG_VSN}" in
    4.3*)
        EMQX_CE_DASHBOARD_VERSION='v4.3.12'
        EMQX_EE_DASHBOARD_VERSION='v4.3.27'
        ;;
    4.4*)
        # keep the above 4.3 untouched, otherwise conflicts!
        EMQX_CE_DASHBOARD_VERSION='v4.4.7'
        EMQX_EE_DASHBOARD_VERSION='v4.4.18'
        ;;
    *)
        echo "Unsupported version $PKG_VSN" >&2
        exit 1
        ;;
esac

RELEASE_ASSET_FILE="emqx-dashboard.zip"

if [ -f 'EMQX_ENTERPRISE' ]; then
    VERSION="${EMQX_EE_DASHBOARD_VERSION}"
    DASHBOARD_PATH='lib-ee/emqx_dashboard/priv'
    DASHBOARD_REPO='emqx-dashboard-web'
    DIRECT_DOWNLOAD_URL="https://github.com/emqx/${DASHBOARD_REPO}/releases/download/${VERSION}/${RELEASE_ASSET_FILE}"
else
    VERSION="${EMQX_CE_DASHBOARD_VERSION}"
    DASHBOARD_PATH='lib-ce/emqx_dashboard/priv'
    DASHBOARD_REPO='emqx-dashboard-frontend'
    DIRECT_DOWNLOAD_URL="https://github.com/emqx/${DASHBOARD_REPO}/releases/download/${VERSION}/${RELEASE_ASSET_FILE}"
fi

case $(uname) in
    *Darwin*) SED="sed -E";;
    *) SED="sed -r";;
esac

version() {
    grep -oE 'github_ref: (.*)' "$DASHBOARD_PATH/www/version" |  $SED 's|github_ref: refs/tags/(.*)|\1|g'
}

if [ -d "$DASHBOARD_PATH/www" ] && [ "$(version)" = "$VERSION" ]; then
    exit 0
fi

curl -L --silent --show-error \
     --header "Accept: application/octet-stream" \
     --output "${RELEASE_ASSET_FILE}" \
     "$DIRECT_DOWNLOAD_URL"

unzip -q "$RELEASE_ASSET_FILE" -d "$DASHBOARD_PATH"
rm -rf "$DASHBOARD_PATH/www"
mv "$DASHBOARD_PATH/dist" "$DASHBOARD_PATH/www"
rm -f "$RELEASE_ASSET_FILE"

EMQX_LIBCE_PATH='lib-ce/'
DGIOT_DASHBOARD_PATH='apps/dgiot_api/priv'
FILESEVER="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"

get_lib_ce(){
   cd ../../../
   cd ${$EMQX_LIBCE_PATH}
   if [ ! -d "emqx_dashboard/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-dashboard.git"  emqx_dashboard
   fi

   if [ ! -d "emqx_modules/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-modules.git"  emqx_modules
   fi

   if [ ! -d "emqx_telemetry/" ]; then
      git clone  -b "v4.4.11" --depth=1 "https://gitee.com/fastdgiot/emqx-telemetry.git"  emqx_telemetry
   fi
}

get_dgiot_dashboard(){
  if [ -d "$DGIOT_DASHBOARD_PATH/www" ]; then
      echo "dgiot_dashboard exist"
  else
    wget ${FILESEVER}/www.tar.gz
    tar xvf www.tar.gz
    mv www "$DGIOT_DASHBOARD_PATH/"
    rm www.tar.gz -rf
    chmod 777 ./scripts/*
  fi
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


 get_apps
 get_dgiot_dashboard
 get_lib_ce
