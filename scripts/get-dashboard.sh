#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

RELEASE_ASSET_FILE="emqx-dashboard.zip"
DASHBOARD_PATH='lib-ce/emqx_dashboard/priv'
DIRECT_DOWNLOAD_URL="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/emqx_dashboard.zip"


if [ -d "$DASHBOARD_PATH/www" ]; then
    exit 0
fi

rm -rf emqx_dashboard.zip
rm -rf www
wget http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/emqx_dashboard.zip
unzip  emqx_dashboard.zip
rm -rf "$DASHBOARD_PATH/www"
mv "./www" "./lib-ce/emqx_dashboard/priv/"
rm -rf emqx_dashboard.zip
chmod 777 ./scripts/*

