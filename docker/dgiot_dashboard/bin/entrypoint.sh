#!/bin/bash

wlanip="173.173.0.10"
certserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/cert"
domain_name=$DOMAIN_NAME
function start_server() {

  systemctl daemon-reload
  systemctl enable nginx

  systemctl start nginx

}

function nginx_domain_name() {
    if [ ! -f /data/dgiot/$DOMAIN_NAME.zip ]; then
       wget ${certserver}/$DOMAIN_NAME.zip -O /data/dgiot/$DOMAIN_NAME.zip  &> /dev/null
    fi
    unzip -o /data/dgiot/$DOMAIN_NAME.zip -d /etc/ssl/certs/
    sed -i "s!{{domain_name}}!$DOMAIN_NAME!g" /data/dgiot/nginx/conf/nginx.conf
}

nginx_domain_name
start_server

exec "$@"
