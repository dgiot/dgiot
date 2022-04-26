#!/usr/bin/env bash
cd ../dgiot-dashboard
#git checkout master
#git pull
#pnpm install &> /dev/null
pnpm build  &> /dev/null
#cd ./dist/
#wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_dashboard.tar.gz &> /dev/null
#tar xf dgiot_dashboard.tar.gz &> /dev/null
#wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_file.tar.gz &> /dev/null
#tar xf dgiot_file.tar.gz &> /dev/null
#wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_swagger.tar.gz &> /dev/null
#tar xf dgiot_swagger.tar.gz &> /dev/null
#rm -rf dgiot_swagger.tar.gz dgiot_file.tar.gz dgiot_dashboard.tar.gz
cd ../../dgiot
#git checkout master
#git pull
#rm ./apps/dgiot_api/priv/www -rf
cp ../dgiot-dashboard/dist/ ./apps/dgiot_api/priv/www -rf
#wget  https://github.com/dgiot/dgiot-dashboard/archive/refs/heads/gh-pages.zip
#unzip  gh-pages.zip
#mv dgiot-dashboard-gh-pages/ www
make run
