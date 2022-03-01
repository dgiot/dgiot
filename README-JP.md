#  DGIOT

[![GitHub Release](https://img.shields.io/github/release/dgiot/dgiot?color=brightgreen)](https://github.com/dgiot/dgiot/releases)
[![Build Status](https://travis-ci.org/dgiot/dgiot.svg)](https://travis-ci.org/dgiot/dgiot)
[![Docker Pulls](https://img.shields.io/docker/pulls/dgiot/dgiot)](https://hub.docker.com/r/dgiot/dgiot)
[![Community](https://img.shields.io/badge/Community-DGIOT-yellow)](https://tech.iotn2n.com)
[![star](https://gitee.com/dgiiot/dgiot/badge/star.svg?theme=gvp)](https://gitee.com/dgiiot/dgiot/stargazers)
[![dgiot/Gitee Feedback](https://gitee.com/dgiiot/dgiot/widgets/widget_card.svg?colors=colors=ffffff,1e252b,323d47,455059,d7deea,99a0ae)](https://gitee.com/dgiiot/dgiot)
[English](./README.md) | [简体中文](./README-CN.md) | [日本語](./README-JP.md) | [русский](./README-RU.md)

*DGIOT*  は中国で最初の軽量オープンソース産業用IoT継続的インテグレーションプラットフォームです

2016年以前、Shuwaチームはインターネットとモバイルインターネットを長年にわたってクロールしていました。 2016年には、モノのインターネットのクロールに参入し始めました。 このオープンソースプラットフォームを通じて長年のクロール経験を共有し、学際的な産業用インターネットプロジェクトを容易にすることを望んでいます。
   + 金持ちのエンジニアがウィンドウの相互作用を通じて、より簡単な要件で産業用インターネットプロジェクトを完了できるようにします
   + ジュニアフロントエンドエンジニアの大多数に、サーバーレス方式を介してより複雑な要件を持つ産業用インターネットプロジェクトを実施させます
   + Python、Java、Go、Cのジュニアバックグラウンドエンジニアが、Webプログラミング開発チャネルを通じて複雑な産業用インターネットプロジェクトに着手できるようにします

# ヴィジョン
  DGIOTチームは、Shuwa IndustrialInternet継続的インテグレーションプラットフォームを通じて次のビジョンを達成したいと考えています。
  +エンジニア、フロントエンドエンジニア、およびジュニアバックオフィスエンジニアを通じて、実際に中小規模の産業用インターネットプロジェクトを1か月以内に完了します。
   +オープンソースコード、無料ソフトウェア、ドキュメント共有、技術認証、製品認証、運用および保守ホスティングなどの複数の方法を通じて、高品質の配信を保証します。
   +技術分野の専門家は、業界の優れた技術フレームワークを統合し続け、ビジネス分野の専門家は、ビジネスモデルとプロセスを最適化し、学際的なオープンプラットフォームを構築し続けます。
   + IoTプラットフォームはついにシンプルで使いやすくなり、ツールの本質に戻ります

# Building

 Bulid *DGIOT* Need Erlang/OTP R21+, Windows download [msys64](https://dgiotdev-1308220533.cos.ap-nanjing.myqcloud.com/msys64.zip),ダウンロード後、ディスクDのルートディレクトリに解凍し、次の図に従って厳密に操作してください。
![dgiotdevtools.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/shuwa_tech/zh/dgiotdevtools.png)

 +  海外でソースコードをダウンロードする
  ```bash
     git clone https://github.com/dgiot/dgiot_dashboard.git
     git clone https://github.com/dgiot/dgiot.git
   ```

 +  中国のダウンロードソースコード
   ```bash
     git clone https://gitee.com/dgiiot/dgiot_dashboard.git
     git clone https://gitee.com/dgiiot/dgiot.git
   ```

 +  中国Linux / Unix / Mac / windowsビルド
  ```bash
    cd dgiot_dashboard
    git pull
    yarn dgiot:install
    yarn build
    cd ../dgiot
    git pull
    rm ./apps/dgiot_api/priv/www -rf
    cp ../dgiot_dashboard/dist/ ./apps/dgiot_api/priv/www -rf
    cp ../dgiot_dashboard/swagger ./apps/dgiot_api/priv/www/ -rf
    make run
    cd ./apps/dgiot_api/priv/www
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_dashboard.tar.gz &> /dev/null
    tar xf dgiot_dashboard.tar.gz &> /dev/null
    wget https://dgiot-release-1306147891.cos.ap-nanjing.myqcloud.com/v4.4.0/dgiot_file.tar.gz &> /dev/null
    tar xf dgiot_file.tar.gz &> /dev/null
 ```
+ Make Debug
 ```
  make DIAGNOSTIC=1
 ```
 *DGIOT* start, you can use a browser to visit  http://localhost to view Dashboard.。

- For a complete list of new features, see [DGIOT Release Notes](https://github.com/dgiot/dgiot/releases)。
- For more information, please visit [DGIOT Website](https://tech.iotn2n.com/)。

## インストールと展開

 + **centos 7.6**

```
wget -q https://gitee.com/dgiiot/dgiot/raw/master/dgiot_install.sh  && sh dgiot_install.sh
```

## コミュニティ

### FAQ

Visit [DGIOT FAQ](https://tech.iotn2n.com/en/backend/) to get help on frequently asked questions

### FAQ

[GitHub Discussions](https://github.com/dgiot/dgiot_server/discussions)
[DGIOT Chinese Q&A Community](https://tech.iotn2n.com/)

### デザインに関わっている

If you have suggestions for improvements to DGIOT, you can submit PR and ISSUE to [EIP](https://github.com/dgiot/eip)

### プラグイン開発

If you want to integrate or develop your own plug-in, refer to [lib-extra/README.md](./lib-extra/README.md)

You are welcome to submit any bugs, issues and feature requests to [dgiot/dgiot](https://github.com/dgiot/dgiot/issues)。

### 私たちに関しては
| contact details       | address                                                                                      |
| -------------- | ----------------------------------------------------------------------------------------- |
| github         | [https://github.com/dgiot](https://github.com/dgiot?from=git)                             |
| gitee          | [https://gitee.com/dgiot](https://gitee.com/dgiiot?from=git)                              |
| Official website           | [https://www.iotn2n.com](https://www.iotn2n.com?from=git)                                 |
| Blog           | [https://tech.iotn2n.com](https://tech.iotn2n.com?from=git)                               |
| IoT access platform | [https://dgiot.iotn2n.com](https://dgiot.iotn2n.com?from=git)                             |
| The public         | ![qrcode.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/qrcode.png) |

### お問い合わせ
You can contact the DGIOT community and developers through the following channels:
- [official](https://www.iotn2n.com)
- [ask](https://ask.iotn2n.com/)
- [Blog](https://tech.iotn2n.com)
- [Twitter](https://twitter.com/)
- [Facebook](https://www.facebook.com/)
- [Reddit](https://www.reddit.com/)
- [Weibo](https://weibo.com)


## プレビューアドレス
[Tencent Cloud preview address](https://dgiotdashboard-8gb17b3673ff6cdd-1253666439.ap-shanghai.app.tcloudbase.com?ftom=git)

## スキャンコードのプレビュー
![dgiot_dashboard.png](http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/wechat/dgiot_dashboard.png)


## オープンソースライセンス
Apache License 2.0, 详见 [LICENSE](./LICENSE)。
