# dgiot parse

parse server erlang 封装



License
-------

Apache License Version 2.0

Author
------

jonhliu


# 安装jdk1.8
#卸载环境中存在的jdk版本
for i in $(rpm -qa | grep jdk | grep -v grep)
do
  echo "Deleting rpm -> "$i
  rpm -e --nodeps $i
done

if [[ ! -z $(rpm -qa | grep jdk | grep -v grep) ]];
then
  echo "-->Failed to remove the defult Jdk."
else
  if [ ! -f /data/jdk-8u291-linux-x64.tar.gz ]; then
    wget http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot4.0/jdk-8u291-linux-x64.tar.gz -O /data/jdk-8u291-linux-x64.tar.gz
  fi
  cd /data
  rm  /usr/local/jdk1.8.0_291 -rf
  tar -zxvf jdk-8u291-linux-x64.tar.gz -C /usr/local
  #配置环境变量
  if ! grep "JAVA_HOME=/usr/local/jdk1.8.0_291" /etc/profile
  then
      echo "export JAVA_HOME=/usr/local/jdk1.8.0_291" >>/etc/profile
	    echo -e 'export JRE_HOME=/usr/local/java/jdk1.8.0_291/jre' >>/etc/profile
      echo -e 'export CLASSPATH=.:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar:$JRE_HOME/lib:$CLASSPATH' >>/etc/profile
      echo -e 'export PATH=$JAVA_HOME/bin:$PATH' >>/etc/profile
      source /etc/profile
  fi
fi
java -version

# 部署 shuwa_report
cd /data
if [ ! -f /data/shuwa_report-4.0.0.zip ]; then
  wget http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot_release/shuwa_report-4.0.0.zip -O /data/shuwa_report-4.0.0.zip
fi
rm /data/shuwa_report-4.0.0 -rf
unzip shuwa_report-4.0.0.zip
# 安装字体
rm /usr/share/fonts/fonts.zip -rf
rm /usr/share/fonts/zhFonts -rf

./shuwa_report-4.0.0/bin/fonts.sh
# 启动
./shuwa_report-4.0.0/bin/startup.sh

sleep 10

# 安装python3.8
# 1、依赖包安装
yum install -y wget curl zlib-devel bzip2-devel openssl-devel ncurses-devel sqlite-devel readline-devel tk-devel gdbm-devel db4-devel libpcap-devel xz-devel libffi-devel
# 2、下载包
if [ ! -f /data/Python-3.8.0.tgz ]; then
  wget http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/dgiot4.0/Python-3.8.0.tgz -O /data/Python-3.8.0.tgz
fi
sleep 10
# 3、解压
tar -xvf Python-3.8.0.tgz
# 4、安装
cd Python-3.8.0
./configure --prefix=/usr/local/python3
make && make install
# 5、建立软连接
/usr/bin/mv python python_bk
/usr/bin/mv pip pip_bk
ln -s /usr/local/python3/bin/python3 /usr/bin/python
ln -s /usr/local/python3/bin/pip3 /usr/bin/pip

# 安装numpy
pip install numpy
# 安装matplotlib
pip install matplotlib
# 安装pylab
pip install pylab
