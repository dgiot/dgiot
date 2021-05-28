#!/usr/bin/env bash

set -euo pipefail

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

if [ -d "_build" ];then
  echo ""
else
#  wget http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/_build.tar.gz -O _build.tar.gz
#  tar xvf _build.tar.gz
  echo "emqx_dashboard"
fi
echo "emqx_dashboard"
