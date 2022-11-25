dockerfiles-centos-nginx
========================

CentOS 7 Dockerfile for nginx, This Dockerfile uses https://www.softwarecollections.org/en/scls/rhscl/rh-nginx18/

This build of nginx listens on port 8080 by default. Please be aware of this
when you launch the container.

error_log and access_log will go to STDOUT.

install docker

    curl -fsSL https://get.docker.com | bash -s docker --mirror Aliyun

也可以使用国内 daocloud 一键安装命令：

    curl -sSL https://get.daocloud.io/docker | sh

start docker

    systemctl start docker

stop docker

    systemctl stop docker

restart  docker

    systemctl restart docker

To build:

Create docker network

    docker network create --subnet=173.173.0.0/24  docker-dgiot

Copy the sources down -

    docker build --rm --tag dgiot/dgiot_db:4.7.1 .

To run:

    docker run --env IS_INTRANET=false -itd --net docker-dgiot --ip 173.173.0.30 --privileged --hostname dgiot_db dgiot/dgiot_db:4.7.1 init

To exec:

    docker exec -it 7503576c7a86 bash

To test:

    curl http://localhost

docker login -u dgiot -p ${passWord}  #replace the docker registry username and password
docker container commit b3b91d654f84 dgiot/dgiot_db:4.7.1
docker commit b3b91d654f84 dgiot/dgiot_db:4.7.1
docker push dgiot/dgiot_db:4.7.1













