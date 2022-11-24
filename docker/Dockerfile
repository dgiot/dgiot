##--------------------------------------------------------------------
## Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##--------------------------------------------------------------------

FROM centos:centos7
MAINTAINER The CentOS Project <cloud-ops@centos.org>

# Labels consumed by Red Hat build service
LABEL Component="dgiot" \
      Name="dgiot/centos7" \
      Version="4.7.0" \
      Release="1"

# Labels could be consumed by OpenShift
LABEL io.k8s.description="dgiot Open source platform for IoT,30 min Quick Deployment,10M devices connection,Carrier level Stability." \
      io.k8s.display-name="dgiot 4.7.0" \
      io.openshift.expose-services="80:http" \
      io.openshift.tags="dgiot"

#VOLUME [ "/data/dgiot/"]

ARG dockerserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/docker"
ARG fileserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"
ARG dgiot="dgiot_n259"
ARG install_dir="/data/dgiot"
ARG domain_name

ADD data /
ADD data/service /data/
ADD bin/entrypoint.sh /usr/bin/
ADD ${dockerserver}/erlang-23.3.4.11-1.el7.x86_64.rpm /data
ADD ${dockerserver}/TDengine-server-3.0.1.7-Linux-x64.rpm /data
ADD ${dockerserver}/taosTools-2.2.9-Linux-x64.rpm /data
ADD ${dockerserver}/dgiot_pg_writer.tar.gz /data
ADD ${dockerserver}/dgiot_parse_server_5.0.tar.gz /data
ADD ${dockerserver}/dgiot_tdengine_mqtt.tar.gz /data
ADD ${dockerserver}/pgsql_12.tar.gz /data
ADD ${dockerserver}/go_fastdfs.tar.gz  /data
ADD ${dockerserver}/nginx.tar.gz /data

ADD ${fileserver}/${dgiot}.tar.gz /data/dgiot/
ADD ${fileserver}/dgiot_file.tar.gz  /data
ADD ${fileserver}/dgiot_dashboard.tar.gz  /data
ADD ${fileserver}/dgiot_swagger.tar.gz  /data
ADD ${fileserver}/libmosquitto.so.1 /usr/lib

RUN yum -y install sudo && \
    yum -y install unzip && \
    yum -y install wget && \
    yum -y install dmidecode && \
    yum -y install openssl && \

#    dgiot
    cd  /data/dgiot && \
    tar xvf ./${dgiot}.tar.gz  && \
    rm ./${dgiot}.tar.gz -rf   && \

    cd  /data && \

#   rpm erlang
    rpm -i ./erlang-23.3.4.11-1.el7.x86_64.rpm  && \
    rm ./erlang-23.3.4.11-1.el7.x86_64.rpm -rf  && \

#   rpm taos
    rpm -i ./TDengine-server-3.0.1.7-Linux-x64.rpm  && \
    rm ./TDengine-server-3.0.1.7-Linux-x64.rpm -rf   && \

    rpm -i ./taosTools-2.2.9-Linux-x64.rpm  && \
    rm ./taosTools-2.2.9-Linux-x64.rpm -rf   && \

    tar xvf ./dgiot_tdengine_mqtt.tar.gz  && \
    rm ./dgiot_tdengine_mqtt.tar.gz -rf   && \
    mv ./dgiot_tdengine_mqtt /usr/sbin/   && \

#   dgiot_pg_writer
    groupadd postgres  && \
    useradd -g postgres postgres  && \
    tar xvf ./dgiot_pg_writer.tar.gz  && \
    rm ./dgiot_pg_writer.tar.gz -rf   && \
    mv ./dgiot_pg_writer /data/dgiot     && \
    chown -R postgres:postgres /data/dgiot/dgiot_pg_writer  && \

#   dgiot_parse_server
    tar xvf ./dgiot_parse_server_5.0.tar.gz  && \
    rm ./dgiot_parse_server_5.0.tar.gz -rf   && \
    mv ./dgiot_parse_server /data/dgiot     && \

#   pgsql
    tar xvf ./pgsql_12.tar.gz  && \
    rm ./pgsql_12.tar.gz -rf   && \
    mv ./pgsql/12/lib64/* /usr/lib64/  && \
    ldconfig   && \
    mv ./pgsql/ /usr/local/  && \

#   go_fastdfs
    tar xvf ./go_fastdfs.tar.gz  && \
    rm ./go_fastdfs.tar.gz -rf   && \
    mv ./go_fastdfs /data/dgiot/  && \

    tar xvf ./dgiot_file.tar.gz  && \
    rm ./dgiot_file.tar.gz -rf   && \
    mv ./dgiot_file /data/dgiot/go_fastdfs/files/  && \

    tar xvf ./dgiot_swagger.tar.gz  && \
    rm ./dgiot_swagger.tar.gz -rf   && \
    mv ./dgiot_swagger /data/dgiot/go_fastdfs/files/  && \

    tar xvf ./dgiot_dashboard.tar.gz  && \
    rm ./dgiot_dashboard.tar.gz -rf   && \
    mv ./dgiot_dashboard /data/dgiot/go_fastdfs/files/  && \

#   nginx
    groupadd nginx  && \
    useradd -g nginx nginx  && \
    tar xvf ./nginx.tar.gz  && \
    rm ./nginx.tar.gz -rf   && \
    mv ./nginx /data/dgiot/  && \

#   service
    mv /data/*.service  /lib/systemd/system/

EXPOSE 80

#CMD ["docker run -itd --privileged -p 80:8080 --hostname dgiot dgiot/dgiot:4.7.0 init"]

ENTRYPOINT ["/usr/bin/entrypoint.sh"]

