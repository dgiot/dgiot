#!/usr/bin/env bash

set -euo pipefail

url="$2"

## ensure enterprise code is not pushed to public repo
if [ -f 'EMQX_ENTERPRISE' ]; then
    if [[ "$url" != *emqx-enterprise* ]]; then
        echo "$(tput setaf 1)error: enterprise_code_to_non_enterprise_repo"
        exit 1
    fi
fi

## 新增：规则体系质量验证
echo "=== 检查规则体系质量 ==="
if [ -f ".clinerules/validate_rules.sh" ]; then
    # 运行规则验证脚本
    if ./.clinerules/validate_rules.sh; then
        echo "$(tput setaf 2)✅ 规则验证通过：规则体系简洁高效有效$(tput sgr0)"
    else
        echo "$(tput setaf 3)⚠️  规则验证警告：规则体系需要改进$(tput sgr0)"
        echo "$(tput setaf 3)提示：验证失败不会阻止push，但建议改进规则质量$(tput sgr0)"
        # 这里不退出，只显示警告
    fi
else
    echo "$(tput setaf 3)⚠️  规则验证脚本不存在，跳过规则检查$(tput sgr0)"
    echo "$(tput setaf 3)提示：请确保.clinerules/validate_rules.sh文件存在$(tput sgr0)"
fi

echo ""

## this triggers a tag vs release version check before pushing a tag
./pkg-vsn.sh
