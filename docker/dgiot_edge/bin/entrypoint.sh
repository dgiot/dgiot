#!/bin/bash

wlanip="173.173.0.20"
certserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/cert"
is_intranet=$IS_INTRANET
domain_name=$DOMAIN_NAME
function start_server() {

  systemctl daemon-reload
  systemctl enable dgiot

  systemctl start dgiot

}

function get_wanip() {
  if [ "${is_intranet}" == "false"  ]; then
     wlanip=`curl whatismyip.akamai.com`
  fi
}


function make_ssl() {
    if [ ! -d /etc/ssl/dgiot/ ]; then
      mkdir -p /etc/ssl/dgiot/
      cd /etc/ssl/dgiot/

      # 生成自签名的CA key和证书
      openssl genrsa -out ca.key 2048 &> /dev/null
      openssl req -x509 -new -nodes -key ca.key -sha256 -days 3650 -subj "/CN=${wlanip}" -out ca.pem &> /dev/null

      # 生成服务器端的key和证书
      openssl genrsa -out server.key 2048 &> /dev/null
      openssl req -new -key ./server.key -out server.csr -subj "/CN=0.0.0.0" &> /dev/null
      openssl x509 -req -in ./server.csr -CA ca.pem -CAkey ca.key -CAcreateserial -out server.pem -days 3650 -sha256 &> /dev/null

      # 生成客户端key和证书
      openssl genrsa -out client.key 2048 &> /dev/null
      openssl req -new -key ./client.key -out client.csr -subj "/CN=0.0.0.0" &> /dev/null
      openssl x509 -req -in ./client.csr -CA ca.pem -CAkey ca.key -CAcreateserial -out client.pem -days 3650 -sha256 &> /dev/null

      cd ${script_dir}/ &> /dev/null
    fi
}


function install_dgiot() {

  #修改 dgiot_parse 连接 配置
  ${csudo} bash -c  "sed -ri '/^parse.parse_server/cparse.parse_server = http://173.173.0.30:1337' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_path/cparse.parse_path = /parse/'  /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_appid/cparse.parse_appid = c15005097b3eea431d5b334ee8ace1a3' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_master_key/cparse.parse_master_key = 349910a0e70a0deace2f5b0ccb30e897' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_js_key/cparse.parse_js_key = 8ef8e49492098ceb871c2c0cc486f00b' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_rest_key/cparse.parse_rest_key = 782482b57aa77215bbf7ee031a82c20a' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"

  echo -e "`date +%F_%T` $LINENO: ${GREEN} install_dgiot dgiot${NC}"
  # 修改dgiot.conf
  ${csudo} bash -c "sed -ri 's!/etc/ssl/certs/domain_name!/etc/ssl/certs/${domain_name}!g' /data/dgiot/dgiot/etc/emqx.conf"

}

get_wanip
make_ssl
install_dgiot
start_server

exec "$@"
