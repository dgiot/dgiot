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
LABEL Component="dgiot_db" \
      Name="dgiot_db/centos7" \
      Version="4.7.1" \
      Release="1"

# Labels could be consumed by OpenShift
LABEL io.k8s.description="dgiot_db Open source platform for IoT,30 min Quick Deployment,10M devices connection,Carrier level Stability." \
      io.k8s.display-name="dgiot_db 4.7.1" \
      io.openshift.tags="dgiot_db"

#VOLUME [ "/data/dgiot/"]

ARG dockerserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/docker"
ARG fileserver="https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0"

ADD data /
ADD data/service /data/
ADD bin/entrypoint.sh /usr/bin/
ADD ${dockerserver}/libmosquitto.so.1 /usr/lib
ADD ${dockerserver}/pgsql_12.tar.gz /data/dgiot/
ADD ${dockerserver}/dgiot_pg_writer.tar.gz /data/dgiot/
ADD ${dockerserver}/redis.tar.gz /data/dgiot/
ADD ${dockerserver}/node.tar.gz /data/dgiot/
ADD ${dockerserver}/dgiot_parse_server_5.0.tar.gz /data/dgiot/
ADD ${dockerserver}/TDengine-server-3.0.1.7-Linux-x64.rpm /data/dgiot/
ADD ${dockerserver}/taosTools-2.2.9-Linux-x64.rpm /data/dgiot/
ADD ${dockerserver}/dgiot_tdengine_mqtt.tar.gz /data/dgiot/
ADD ${dockerserver}/go_fastdfs.tar.gz  /data/dgiot/

ADD ${fileserver}/dgiot_file.tar.gz  /data/dgiot/

RUN yum -y install sudo && \
    yum -y install unzip && \
    yum -y install wget && \
    yum -y install dmidecode && \
    yum -y install openssl && \

    cd  /data/dgiot/ && \

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
    chown -R postgres:postgres /data/dgiot/dgiot_pg_writer  && \

#   pgsql
    tar xvf ./pgsql_12.tar.gz  && \
    rm ./pgsql_12.tar.gz -rf   && \
    mv ./pgsql/12/lib64/* /usr/lib64/  && \
    ldconfig   && \
    mv ./pgsql/ /usr/local/  && \

#   node
    tar xvf ./node.tar.gz  && \
    rm ./node.tar.gz -rf   && \
    mv ./node /usr/sbin/     && \

#   redis
    tar xvf ./redis.tar.gz  && \
    rm ./redis.tar.gz -rf   && \
    mv ./redis /usr/sbin/     && \

#   dgiot_parse_server
    tar xvf ./dgiot_parse_server_5.0.tar.gz  && \
    rm ./dgiot_parse_server_5.0.tar.gz -rf   && \

#   go_fastdfs
    tar xvf ./go_fastdfs.tar.gz  && \
    rm ./go_fastdfs.tar.gz -rf   && \

    tar xvf ./dgiot_file.tar.gz  && \
    rm ./dgiot_file.tar.gz -rf   && \
    mv ./dgiot_file /data/dgiot/go_fastdfs/files/  && \

#   service
    mv /data/*.service  /lib/systemd/system/  && \

    yum clean all


#CMD ["docker run -itd --privileged -p 80:8080 --hostname dgiot dgiot/dgiot:4.7.0 init"]

ENTRYPOINT ["/usr/bin/entrypoint.sh"]

