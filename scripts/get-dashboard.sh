#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

EMQX_DASHBOARD_PATH='lib-ce/emqx_dashboard/priv'
DGIOT_DASHBOARD_PATH='apps/dgiot_api/priv'
FILESEVER="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"

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


