#!/usr/bin/env bash
flag=$1
cd ../dgiot-dashboard
git checkout master
git pull
# Judge whether there is a node_ Modules folder
if [ ! -d "node_modules" ]; then
  echo "node_modules not found, install node_modules..."
  pnpm install &> /dev/null
fi
# Judge whether dist directory is empty
if [ ! -d "dist" ]; then
  echo "dist not found, build..."
  pnpm build  &> /dev/null
fi

cd ./dist/
# Judge apps /dgiot_ api/priv/www/dgiot_dgiot_dashboard  Does the folder exist
if [ ! -d "../dgiot/apps/dgiot_api/priv/www/dgiot_dashboard" ]; then
   echo "dgiot_dashboard not found, copy..."
   wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_dashboard.tar.gz &> /dev/null
   tar xf dgiot_dashboard.tar.gz &> /dev/null
fi
# Judge apps /dgiot_ api/priv/www/dgiot_file Does the folder exist
if [ ! -d "../dgiot/apps/dgiot_api/priv/www/dgiot_file" ]; then
   echo "dgiot_file not found, copy..."
   wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_file.tar.gz &> /dev/null
  tar xf dgiot_file.tar.gz &> /dev/null
fi
# Judge apps /dgiot_api/priv/www/dgiot_dgiot_swagger Does the folder exist
if [ ! -d "../dgiot/apps/dgiot_api/priv/www/dgiot_swagger" ]; then
   echo "dgiot_swagger not found, copy..."
   wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_swagger.tar.gz &> /dev/null
   tar xf dgiot_swagger.tar.gz &> /dev/null
fi
# Judge apps  dgiot_swagger.tar.gz dgiot_file.tar.gz dgiot_dashboard.tar.gz Does the folder exist
if [ -f "dgiot_swagger.tar.gz" ]; then
  rm -rf dgiot_swagger.tar.gz
fi
if [ -f "dgiot_file.tar.gz" ]; then
  rm -rf dgiot_file.tar.gz
fi
if [ -f "dgiot_dashboard.tar.gz" ]; then
  rm -rf dgiot_dashboard.tar.gz
fi
cd ../../dgiot
git checkout master
git pull
# Determine whether to use git page files
if [ -n "$flag" ] && [ "$flag" = "github" ]; then
  echo "install git-page..."
  wget  https://github.com/dgiot/dgiot-dashboard/archive/refs/heads/gh-pages.zip
  unzip  gh-pages.zip
  mv dgiot-dashboard-gh-pages/ www
fi
rm ./apps/dgiot_api/priv/www -rf
cp ../dgiot-dashboard/dist/ ./apps/dgiot_api/priv/www -rf
make run
