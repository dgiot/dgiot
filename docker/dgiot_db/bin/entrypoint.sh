#!/bin/bash

wlanip="173.173.0.30"
certserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/cert"
is_intranet=$IS_INTRANET
function start_server() {

  systemctl daemon-reload
  systemctl enable dgiot_pg_writer
  systemctl enable dgiot_redis
  systemctl enable gofastdfs
  systemctl enable taosd
  systemctl enable taosadapter
  systemctl enable dgiot_parse_server
#  systemctl enable dgiot_tdengine_mqtt

  systemctl start dgiot_pg_writer
  systemctl start dgiot_redis
  systemctl start gofastdfs
  systemctl start taosd
  systemctl start taosadapter
  sleep 5
  systemctl start dgiot_parse_server
#  systemctl start dgiot_tdengine_mqtt

}

function get_wanip() {
  if [ "${is_intranet}" == "false"  ]; then
     wlanip=`curl whatismyip.akamai.com`
  fi
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
  ${csudo} bash -c "echo  'DASHBOARD_USER = dgiot'                                         >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_PASS = 122778197a8432308222feaed18f27c5'              >> ${parseconfig}"
  ${csudo} bash -c "echo  'DASHBOARD_ENCPASS = false'                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 数据配置'                                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'DATABASE = postgres://postgres:dgiot1344@127.0.0.1:7432/parse'  >> ${parseconfig}"
  ${csudo} bash -c "echo  'REDIS_SOCKET = redis://127.0.0.1:16379/0'                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'REDIS_CACHE = redis://127.0.0.1:16379/1'                        >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 邮箱配置'                                                      >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_FROM_ADDRESS = lsxredrain@163.com'                        >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_FROM_NAME = 系统通知'                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SENDMAIL = true'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_HOST = smtp.163.com'                                  >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_PORT = 465'                                           >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_SECURE = true'                                        >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_USER = lsxredrain'                                    >> ${parseconfig}"
  ${csudo} bash -c "echo  'EMAIL_SMTP_PASS = youpasswd'                                     >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 接口配置'                                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_APPID = c15005097b3eea431d5b334ee8ace1a3'                    >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_MASTER = 349910a0e70a0deace2f5b0ccb30e897'                   >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_READONLY_MASTER = 223a1db3c8ee4bc84225f14761ec3e4e'          >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_FILE = 0b955b75bd5534c7f12d181346597dd3'                     >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_CLIENT = 4fde3ec4ec1773a042187cf29ee6d90f'                   >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_JAVASCRIPT = 8ef8e49492098ceb871c2c0cc486f00b'               >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_RESTAPI = 782482b57aa77215bbf7ee031a82c20a'                  >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_DOTNET = 8825c18121a9b0409c52ada6b4bdeb14'                   >> ${parseconfig}"
  ${csudo} bash -c "echo  'KEY_WEBHOOK = 4c158d05a184b5883192c7e25b159aa3'                  >> ${parseconfig}"
  ${csudo} bash -c "echo  '# 会话配置'                                                       >> ${parseconfig}"
  ${csudo} bash -c "echo  'SESSION_LENGTH = 604800'                                         >> ${parseconfig}"
}

get_wanip
create_parse_env
start_server

exec "$@"
