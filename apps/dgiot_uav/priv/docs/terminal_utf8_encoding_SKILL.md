---
name: terminal_utf8_encoding
description: 终端UTF-8编码设置技能，解决Erlang/DGIOT开发中的中文显示问题，包括环境变量配置、emqx eval中文执行、远程shell连接等场景
version: 1.0.0
author: CodeBuddy
created_date: 2026-03-22
category: productivity
tags: [utf8, encoding, terminal, chinese, dgiot, erlang]
trigger_phrases:
  - 终端编码
  - UTF-8设置
  - 中文显示
  - LANG环境变量
  - LC_ALL设置
  - emqx eval中文
  - 终端乱码
  - 中文查询
---

# 终端UTF-8编码设置技能

本技能解决Erlang/DGIOT开发中的中文显示和执行问题，提供完整的终端UTF-8编码配置方案。

## 快速开始

### 触发方式

当用户提及以下内容时自动触发：
- "终端编码"、"UTF-8设置"
- "中文显示"、"终端乱码"
- "LANG环境变量"、"LC_ALL设置"
- "emqx eval中文"、"中文查询"

### 核心功能

1. **环境变量配置**：自动设置LANG和LC_ALL环境变量
2. **emqx eval中文执行**：解决shell转义导致的中文执行失败问题
3. **远程Shell连接**：通过remsh连接远程节点执行中文命令
4. **HTTP API查询**：通过REST API执行中文查询
5. **持久化配置**：将配置写入.bashrc永久生效

## 环境变量配置

### 临时生效（当前会话）

```bash
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
```

### 永久生效（写入.bashrc）

```bash
echo 'export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8' >> ~/.bashrc
source ~/.bashrc
```

### 验证配置

```bash
echo "LANG=$LANG"
echo "LC_ALL=$LC_ALL"
```

## emqx eval中文执行方案

### 问题说明

`emqx eval`命令存在shell转义问题，无法直接执行包含中文字符的Erlang代码。

### 解决方案

#### 方案1：创建Erlang模块文件

在`apps/dgiot/src/`目录下创建查询模块：

```erlang
-module(test_query).
-export([run/0]).

run() ->
    Name = <<"总测1_飞控版本号检查"/utf8>>,
    Result = dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"name">> => Name}, <<"limit">> => 1}),
    io:format("~p~n", [Result]).
```

然后执行：

```bash
# 热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'

# 执行查询
_build/emqx/rel/emqx/bin/emqx eval 'test_query:run().'
```

#### 方案2：远程Shell连接（remsh）

```bash
# 获取Cookie
cd /root/gitee/dgiot
grep -r "cookie" _build/emqx/rel/emqx/etc/ | head -1

# 连接远程节点
erl -name temp@127.0.0.1 -setcookie emqxsecretcookie -remsh emqx@127.0.0.1
```

在Erlang Shell中执行：

```erlang
Name = <<"总测1_飞控版本号检查"/utf8>>,
dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"name">> => Name}, <<"limit">> => 1}).
```

#### 方案3：通过HTTP API查询

```bash
# 获取Token
curl -X POST http://127.0.0.1:5080/iotapi/sessions \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin"}'

# 查询设备（中文需要URL编码）
curl -s "http://127.0.0.1:5080/iotapi/devices?where=%7B%22name%22%3A%22%E6%80%BB%E6%B5%8B1_%E9%A3%9E%E6%8E%A7%E7%89%88%E6%9C%AC%E5%8F%B7%E6%A3%80%E6%9F%A5%22%7D" \
  -H "X-Parse-Session-Token: YOUR_TOKEN"
```

## DGIOT系统状态检查

### 检查EMQX进程

```bash
ps aux | grep emqx | grep -v grep
```

### 检查DGIOT应用

```bash
_build/emqx/rel/emqx/bin/emqx eval 'application:which_applications().' | grep dgiot
```

### 检查监听端口

```bash
netstat -tlnp | grep emqx
# 常用端口：18083(管理台), 8080/8081/5080(HTTP API), 1883(MQTT)
```

## 常见问题

### Q1: emqx eval执行中文命令失败

**原因**：shell转义问题  
**解决方案**：使用本技能提供的方案1-3

### Q2: Erlang Shell中文显示乱码

**原因**：终端编码未设置  
**解决方案**：执行`export LANG=en_US.UTF-8`

### Q3: io:format打印中文显示异常

**原因**：未使用UTF-8编码的二进制  
**解决方案**：使用`<<"中文"/utf8>>`格式，打印时使用`~ts`

### Q4: 日志中中文显示乱码

**原因**：日志系统编码问题  
**解决方案**：参考`chinese_printing_solution`技能

## 相关技能

- **chinese_printing_solution**：处理Erlang代码中的中文打印输出
- **erlang_chinese_utf8**：Erlang中文字符串UTF-8编码规范
- **dgiot_compile_debug**：DGIOT编译调试技能

## 维护信息

- 创建日期：2026-03-22
- 版本：1.0.0
- 作者：CodeBuddy
