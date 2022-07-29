#!/usr/bin/env bash
#wget -qO baiduMap_upgrade.sh https://gitee.com/dgiiot/dgiot/raw/master/baiduMap_upgrade.sh  && chmod 777 baiduMap_upgrade.sh && sh baiduMap_upgrade.sh 'WpeAb6pL4tsX2ZVd11156GHbO9Ut6c4HZhG'

#if [ ! -d "dgiot-dashboard" ]; then
echo "当前进度: 安装node"
mkdir /usr/local/node/
cd /usr/local/node/
wget https://npm.taobao.org/mirrors/node/v14.17.6/node-v14.17.6-linux-x64.tar.gz
tar -zxvf node-v14.17.6-linux-x64.tar.gz
rm -rf node-v14.17.6-linux-x64.tar.gz
ln -s /usr/local/node/node-v14.17.6-linux-x64/bin/npm /usr/local/bin/npm
ln -s /usr/local/node/node-v14.17.6-linux-x64/bin/node /usr/local/bin/node
node -v
#fi

mapKey=$1
echo "你输入的百度地图key为: $mapKey"
cd /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv

if [ ! -d "dgiot-dashboard" ]; then
  echo "当前进度: 克隆dgiot-dashboard"
  git clone --depth 1 https://gitee.com/dgiiot/dgiot-dashboard.git dgiot-dashboard
fi

cd dgiot-dashboard
if [ ! -d "node_modules" ]; then
  echo "进度: 安装依赖"
  echo "node_modules not found, install node_modules..."
  npm install
fi
echo "进度: 替换key"
sed -i "s/'WpeAb6pL4tsX2ZVd56GHbO9Ut6c4HZhG'/$mapKey/g" /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/dgiot-dashboard/src/config/secret.config.js
echo "进度: 打包编译"
npm build
echo "进度: 拷贝文件夹dist"
mv /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/wwwback
mkdir /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www
cp -r /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/dgiot-dashboard/dist/* /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/
echo "进度: 删除下载文件dgiot-dashboard"
#rm -rf /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/dgiot-dashboard
echo '替换百度地图key完成'
