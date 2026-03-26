# DG-IoT安全规则

## 概述

确保公开代码和私有代码的严格分离，保护敏感信息安全。

## 核心原则

### 1. 目录分类

- **公开目录** (`docs/`)：只包含通用技术文档，使用示例值
- **私有插件目录** (`apps/dgiot_<private>/`)：客户特定实现，使用占位符
- **通用插件目录** (`apps/dgiot_<generic>/`)：公开通用功能

### 2. 敏感信息管理

- **禁止硬编码**：敏感信息不得硬编码在源代码中
- **使用占位符**：配置文件使用 `{{VARIABLE_NAME}}` 格式
- **环境变量**：通过环境变量注入敏感信息

### 3. 敏感信息定义

- **网络信息**：实际IP地址、端口、域名
- **认证信息**：用户名、密码、API密钥、令牌
- **业务信息**：客户名称、项目代码、产品ID

### 4. 数据库操作规则

- **Parse库操作**：
  - 禁止在Product、Device等对象中添加自定义字段
  - 只能操作已有的标准字段
  - 使用content字段存储业务配置
  - 时序数据存储到TDengine
- **数据一致性**：
  - 保持数据库结构的一致性
  - 禁止随意修改数据模型
  - 确保数据同步和迁移的安全性

## 最佳实践

### 1. 文档编写

```markdown
# 正确示例 - 使用通用描述
## 网络架构设计
- 设备网络：192.168.100.0/24（示例）
- 管理网络：192.168.1.0/24（示例）

# 错误示例 - 包含实际信息
## 客户项目网络
- 摄像头服务器：192.168.100.2:9017
- API密钥：your_app_key_here
```

### 2. 配置模板

```erlang
% apps/dgiot_client/etc/dgiot_client.conf.template
appHost = {{APP_HOST}}
appKey = {{APP_KEY}}
appSecret = {{APP_SECRET}}
```

### 3. 部署脚本

```bash
#!/bin/bash
# deploy_client.sh
export APP_HOST=${CLIENT_APP_HOST:-"http://192.168.100.2:9017"}
export APP_KEY=${CLIENT_APP_KEY:-"your_app_key"}
envsubst < apps/dgiot_client/etc/dgiot_client.conf.template > apps/dgiot_client/etc/dgiot_client.conf
```

### 4. Git管理

```
# .gitignore配置
apps/*/etc/*.conf
*.secret
*.key
.env
.env.local
```

## 检查清单

- [ ] 公开目录不包含敏感信息
- [ ] 配置文件使用占位符
- [ ] 私有代码在私有目录中
- [ ] 文档使用示例值
- [ ] 定期安全扫描
- [ ] **Parse库操作未添加自定义字段**
- [ ] **数据库操作遵循标准模型**

## 更新记录

- 2025-12-19：创建精简版安全规则
- 2026-03-16：添加数据库操作规则，禁止Parse库随意添加字段
