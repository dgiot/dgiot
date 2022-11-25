#!/bin/bash

wlanip="173.173.0.10"
certserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/cert"
is_intranet=$IS_INTRANET
domain_name=$DOMAIN_NAME
function start_server() {

  systemctl daemon-reload
  systemctl enable dgiot_pg_writer
  systemctl enable dgiot_redis
  systemctl enable gofastdfs
  systemctl enable dgiot_parse_server
  systemctl enable taosd
  systemctl enable taosadapter
  systemctl enable dgiot
  systemctl enable dgiot_tdengine_mqtt
  systemctl enable nginx

  systemctl start dgiot_pg_writer
  systemctl start dgiot_redis
  systemctl start gofastdfs
  systemctl start dgiot_parse_server
  systemctl start taosd
  systemctl start taosadapter
#
  systemctl start dgiot
  systemctl start dgiot_tdengine_mqtt
  systemctl start nginx

}

function get_wanip() {
  if [ "${is_intranet}" == "false"  ]; then
     wlanip=`curl whatismyip.akamai.com`
  fi
}

function nginx_domain_name() {
    if [ ! -f /data/dgiot/$DOMAIN_NAME.zip ]; then
       wget ${certserver}/$DOMAIN_NAME.zip -O /data/dgiot/$DOMAIN_NAME.zip  &> /dev/null
    fi
    unzip -o /data/dgiot/$DOMAIN_NAME.zip -d /etc/ssl/certs/
    sed -i "s!{{domain_name}}!$DOMAIN_NAME!g" /data/dgiot/nginx/conf/nginx.conf
}

function create_parse_env(){
  ###  配置dgiot_parse_server配置参数
  parseconfig=/data/dgiot/dgiot_parse_server/script/.env
  ${csudo} bash -c "echo '# 基础配置'                                                      > ${parseconfig}"
  ${csudo} bash -c "echo 'SERVER_NAME = dgiot_parse_server'                                >> ${parseconfig}"
  ${csudo} bash -c "echo 'SERVER_PORT = 1337'                                              >> ${parseconfig}"
  ${csudo} bash -c "echo 'SERVER_DOMAIN = http://${wlanip}:1337'                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'SERVER_PUBLIC = http://${wlanip}:1337'                          >> ${parseconfig}"
  ${csudo} bash -c "echo  'SERVER_PATH = /parse'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'GRAPHQL_PATH = http://${wlanip}:1337/graphql'                   >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 管理配置'                                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_PATH = /dashboard'                                    >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_USER = ${parse_user}'                                 >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_PASS = ${parse_pwd}'                                  >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_ENCPASS = false'                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 数据配置'                                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'DATABASE = postgres://postgres:dgiot1344@127.0.0.1:7432/parse'  >> ${parseconfig}"
  ${csudo} bash -c "echo  'REDIS_SOCKET = redis://127.0.0.1:16379/0'                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'REDIS_CACHE = redis://127.0.0.1:16379/1'                        >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 邮箱配置'                                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_FROM_ADDRESS = lsxredrain@163.com'                         >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_FROM_NAME = 系统通知'                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SENDMAIL = true'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_HOST = smtp.163.com'                                  >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_PORT = 465'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_SECURE = true'                                        >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_USER = lsxredrain'                                    >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_PASS = youpasswd'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 接口配置'                                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_APPID = ${parse_appid}'                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_MASTER = ${parse_master}'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_READONLY_MASTER = ${parse_readonly_master}'                   >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_FILE = ${parse_file}'                                         >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_CLIENT = ${parse_client}'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_JAVASCRIPT = ${parse_javascript}'                             >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_RESTAPI = ${parse_restapi}'                                   >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_DOTNET = ${parse_dotnet}'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_WEBHOOK = ${parse_webhook}'                                   >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 会话配置'                                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'SESSION_LENGTH = 604800'                                          >> ${parseconfig}"
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

function dgiot_password() {
  pg_pwd=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_user=dgiot
  parse_pwd=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_appid=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_master=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_readonly_master=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_file=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_client=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_javascript=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_restapi=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_dotnet=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
  parse_webhook=`openssl rand -hex 8 | md5sum | cut -f1 -d ' '`
}

function install_dgiot() {

  #修改 dgiot_parse 连接 配置
  ${csudo} bash -c  "sed -ri '/^parse.parse_server/cparse.parse_server = http://127.0.0.1:1337' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_path/cparse.parse_path = /parse/'  /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_appid/cparse.parse_appid = ${parse_appid}' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_master_key/cparse.parse_master_key = ${parse_master}' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_js_key/cparse.parse_js_key = ${parse_javascript}' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"
  ${csudo} bash -c  "sed -ri '/^parse.parse_rest_key/cparse.parse_rest_key = ${parse_restapi}' /data/dgiot/dgiot/etc/plugins/dgiot_parse.conf"

  echo -e "`date +%F_%T` $LINENO: ${GREEN} install_dgiot dgiot${NC}"
  # 修改dgiot.conf
  ${csudo} bash -c "sed -ri 's!/etc/ssl/certs/domain_name!/etc/ssl/certs/${domain_name}!g' /data/dgiot/dgiot/etc/emqx.conf"

}

get_wanip
make_ssl
dgiot_password
create_parse_env
install_dgiot
nginx_domain_name
start_server

exec "$@"
