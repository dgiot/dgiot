#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

DASHBOARD_PATH='lib-ce/emqx_dashboard/priv'
DIRECT_DOWNLOAD_URL="http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/v4.4.0/www.tar.gz"

if [ -d "$DASHBOARD_PATH/www" ]; then
    exit 0
fi

wget ${DIRECT_DOWNLOAD_URL}
tar xvf www.tar.gz
mv www "$DASHBOARD_PATH/"
rm www.tar.gz -rf
chmod 777 ./scripts/*

