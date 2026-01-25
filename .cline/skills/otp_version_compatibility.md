# OTP版本兼容性解决方案

## 问题描述
在编译DGIOT项目时遇到OTP版本不兼容错误：
```
OTP release 26 or later is required. Version in use: 24.3.2
```

## 根本原因
项目依赖的某些库或工具需要OTP 26或更高版本，但当前系统安装的是OTP 24.3.2。

## 解决方案

### 方案1：升级OTP版本（推荐）
```bash
# 检查当前OTP版本
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

# 安装OTP 26或更高版本
# 使用kerl管理多个OTP版本
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl
./kerl update releases

# 查看可用版本
./kerl list releases

# 安装OTP 26
./kerl build 26.2.2 26.2.2
./kerl install 26.2.2 ~/otp/26.2.2

# 激活版本
. ~/otp/26.2.2/activate
```

### 方案2：修改项目配置支持低版本
如果无法升级OTP，可以尝试修改项目配置：

1. **修改rebar.config**:
```erlang
%% 降低最低OTP版本要求
{minimum_otp_vsn, "24"}.
```

2. **检查依赖兼容性**:
```bash
# 查看哪些依赖需要高版本OTP
grep -r "minimum_otp_vsn" apps/ lib-*/ 2>/dev/null
```

3. **使用兼容性补丁**:
```erlang
%% 在应用配置中添加兼容性设置
{erl_opts, [
    {platform_define, "24", compatibility_mode}
]}.
```

### 方案3：使用Docker容器
```bash
# 使用包含OTP 26的Docker镜像
docker run -it --rm -v $(pwd):/app erlang:26-alpine

# 在容器中编译
cd /app
make compile
```

### 方案4：开发环境绕过
对于开发环境，可以临时绕过版本检查：

```bash
# 设置环境变量跳过版本检查
export REBAR_SKIP_OTP_VSN_CHECK=1

# 或者修改rebar3配置
echo '{skip_otp_vsn_check, true}.' > ~/.config/rebar3/rebar.config
```

## 预防措施

### 1. 版本管理最佳实践
```bash
# 使用asdf管理多个Erlang版本
asdf plugin-add erlang
asdf install erlang 26.2.2
asdf global erlang 26.2.2

# 项目级版本配置
echo "erlang 26.2.2" > .tool-versions
```

### 2. CI/CD配置
```yaml
# GitHub Actions示例
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26.2'
          rebar3-version: '3.22'
```

### 3. 开发环境检查脚本
```bash
#!/bin/bash
# scripts/check_environment.sh

REQUIRED_OTP="26"
CURRENT_OTP=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell)

if [ "$CURRENT_OTP" -lt "$REQUIRED_OTP" ]; then
    echo "错误: 需要OTP $REQUIRED_OTP或更高版本，当前版本: $CURRENT_OTP"
    echo "请使用以下方法之一："
    echo "1. 升级OTP版本"
    echo "2. 使用Docker容器"
    echo "3. 设置REBAR_SKIP_OTP_VSN_CHECK=1"
    exit 1
fi

echo "环境检查通过: OTP $CURRENT_OTP"
```

## 故障排除

### 常见错误及解决

1. **错误**: `{error,{include,lib}}`
   **解决**: 检查include路径，确保头文件存在

2. **错误**: `undefined function`
   **解决**: 检查函数导出，确保模块依赖正确

3. **错误**: `badarg`
   **解决**: 检查函数参数类型和值

### 调试命令
```erlang
%% 检查OTP版本详细信息
erl -eval 'io:format("OTP Release: ~p~n", [erlang:system_info(otp_release)]), halt().'

%% 检查代码路径
erl -eval 'io:format("Code Paths: ~p~n", [code:get_path()]), halt().'

%% 检查应用版本
erl -eval 'io:format("Kernel: ~p~n", [application:get_key(kernel, vsn)]), halt().'
```

## 相关技能
- [dgiot_compile_debug](./dgiot_compile_debug.md): 编译调试技能
- [erlang_chinese_utf8](./erlang_chinese_utf8.md): Erlang中文编码解决方案
- [engineering_compile_perspective](./engineering_compile_perspective.md): 工程编译视角

## 更新历史
- v1.0.0 (2026-01-23): 初始版本，包含OTP版本兼容性解决方案
- v1.1.0 (2026-01-23): 添加预防措施和故障排除

---

*本技能总结了DGIOT项目中OTP版本兼容性问题的解决方案和最佳实践。*