 http://localhost http://localhost http://localhost#  DGIOT

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Build Status](https://travis-ci.org/dgiot/dgiot.svg)](https://travis-ci.org/dgiot/dgiot)
[![Coverage Status](https://coveralls.io/repos/github/dgiot/dgiot/badge.svg)](https://coveralls.io/github/dgiot/dgiot)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)

[English](./README.md) | [简体中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)

**DGIOT**  первая легкая промышленная платформа непрерывной интеграции Интернета вещей с открытым исходным кодом в Китае

До 2016 года команда Shuwa много лет ползла по Интернету и мобильному Интернету. В 2016 году он начал ползать в Интернет вещей. Он надеется поделиться многолетним опытом сканирования с помощью этой платформы с открытым исходным кодом и упростить многопрофильные промышленные интернет-проекты.
   + Позвольте богатым инженерам выполнять промышленные интернет-проекты с более простыми требованиями через оконное взаимодействие
   + Позвольте большинству младших интерфейсных инженеров выполнять промышленные интернет-проекты с более сложными требованиями бессерверным методом
   + Позвольте младшим инженерам Python, Java, Go и C выполнять сложные промышленные интернет-проекты через каналы разработки веб-программирования

# Зрение
  Команда DGIOT надеется достичь следующих видений с помощью платформы непрерывной интеграции Shuwa Industrial Internet.
  + Через инженеров, интерфейсных инженеров и младших инженеров бэк-офиса для фактического завершения малых и средних промышленных интернет-проектов не более чем за 1 месяц
  + Обеспечение высококачественной доставки с помощью различных методов, таких как открытый исходный код, бесплатное программное обеспечение, совместное использование документов, техническая сертификация, сертификация продукции, хостинг для эксплуатации и обслуживания и т. Д.
  + Эксперты в технической области продолжают интегрировать превосходную техническую базу отрасли, а эксперты в области бизнеса продолжают оптимизировать бизнес-модели и процессы и создавать междисциплинарную открытую платформу
  + Платформа IoT, наконец, может быть простой и удобной в использовании, возвращаясь к сути инструментов

# Строительство

  Bulid * DGIOT * Требуется Erlang / OTP R21 +, загрузка Windows [msys64](https://dgiotdev-1308220533.cos.ap-nanjing.myqcloud.com/msys64.zip), После загрузки распакуйте его в корневой каталог диска D, и работайте строго в соответствии со следующим рисунком ：
![dgiotdevtools.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiotdevtools.png)

+ Скачать исходный код за границу
  ```bash
     git clone https://github.com/dgiot/dgiot_dashboard.git
     git clone https://github.com/dgiot/dgiot.git
   ```

+ Китай скачать исходный код
   ```bash
     git clone https://gitee.com/dgiiot/dgiot_dashboard.git
     git clone https://gitee.com/dgiiot/dgiot.git
   ```

+ Китайская Linux / Unix / Mac / Windows Build
  ```bash
    cd dgiot-dashboard
    git pull
    pnpm install
    pnpm build
    cd ../dgiot
    git pull

    rm ./apps/dgiot_api/priv/www -rf
    cp ../dgiot-dashboard/dist/ ./apps/dgiot_api/priv/www -rf
    make run
    cd ./apps/dgiot_api/priv/www
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_dashboard.tar.gz &> /dev/null
    tar xf dgiot_dashboard.tar.gz &> /dev/null
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_file.tar.gz &> /dev/null
    tar xf dgiot_file.tar.gz &> /dev/null
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_swagger.tar.gz &> /dev/null
    tar xf dgiot_swagger.tar.gz &> /dev/null
 ```
+ Make Debug
 ```
  make DIAGNOSTIC=1
 ```
 *DGIOT* start, you can use a browser to visit http://localhost:5080 to view Dashboard.。

- Полный список новых функций см. [DGIOT Release Notes](https://github.com/dgiot/dgiot/releases)。
- Для получения дополнительной информации, пожалуйста, посетите [DGIOT Website](https://tech.iotn2n.com/)。
## Установка и развертывание

 + **centos 7.6**

```
wget -q https://gitee.com/dgiiot/dgiot/raw/master/dgiot_install.sh  && sh dgiot_install.sh
```

## Сообщество

### FAQ

Visit [DGIOT FAQ](https://tech.iotn2n.com/en/backend/) to get help on frequently asked questions

### FAQ

[GitHub Discussions](https://github.com/dgiot/dgiot_server/discussions)
[DGIOT Chinese Q&A Community](https://tech.iotn2n.com/)

### Участвовал в дизайне

Если у вас есть предложения по улучшению DGIOT, вы можете отправить PR и ISSUE на [EIP](https://github.com/dgiot/eip)

### Plug-in development

Если вы хотите интегрировать или разработать собственный плагин, обратитесь к [lib-extra/README.md](./lib-extra/README.md)

Вы можете отправлять любые ошибки, проблемы и запросы функций по адресу [dgiot/dgiot](https://github.com/dgiot/dgiot/issues)。

### О нас
| Контактная информация      | адрес                                                                                   |
| -------------- | ----------------------------------------------------------------------------------------- |
| github         | [https://github.com/dgiot](https://github.com/dgiot?from=git)                             |
| gitee          | [https://gitee.com/dgiot](https://gitee.com/dgiiot?from=git)                              |
| Official website           | [https://www.iotn2n.com](https://www.iotn2n.com?from=git)                                 |
| Blog           | [https://tech.iotn2n.com](https://tech.iotn2n.com?from=git)                               |
| IoT access platform | [https://dgiot.iotn2n.com](https://dgiot.iotn2n.com?from=git)                             |
| The public         | ![qrcode.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/qrcode.png) |

### Связаться с нами
Вы можете связаться с сообществом DGIOT и разработчиками по следующим каналам:
- [official](https://www.iotn2n.com)
- [ask](https://ask.iotn2n.com/)
- [Blog](https://tech.iotn2n.com)
- [Twitter](https://twitter.com/)
- [Facebook](https://www.facebook.com/)
- [Reddit](https://www.reddit.com/)
- [Weibo](https://weibo.com)


## Адрес для предварительного просмотра
[Tencent Cloud preview address](https://dgiotdashboard-8gb17b3673ff6cdd-1253666439.ap-shanghai.app.tcloudbase.com?ftom=git)

## Предварительный просмотр кода сканирования
![dgiot_dashboard.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/dgiot_dashboard.png)


## Лицензия с открытым исходным кодом
Apache License 2.0, Смотрите подробности [LICENSE](./LICENSE)。
