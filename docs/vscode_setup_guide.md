# VSCode开发环境配置指南

## 必需扩展
1. **核心扩展**：
   - [Erlang LS](https://marketplace.visualstudio.com/items?itemName=erlang-ls.erlang-ls)：Erlang语言支持
   - [Remote - WSL](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl)：WSL开发支持

2. **AI辅助**：
   - [GitHub Copilot](https://marketplace.visualstudio.com/items?itemName=GitHub.copilot)
   - [TabNine](https://marketplace.visualstudio.com/items?itemName=TabNine.tabnine-vscode)

## 配置步骤
1. **WSL环境准备**：
```bash
# 在~/.bashrc中添加
alias emqx_ctl='_build/emqx/rel/emqx/bin/emqx_ctl'
alias hotcompile='f() { rpc:call(emqx@127.0.0.1, dgiot_plugin, compile, [$1]). }; f'
```

2. **VSCode设置**（settings.json）：
```json
{
  "erlang.format.enable": true,
  "editor.formatOnSave": true,
  "files.exclude": {
    "**/_build": true
  }
}
```

3. **任务配置**（tasks.json）：
```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Hot Compile",
      "type": "shell",
      "command": "hotcompile dgiot_mcp",
      "group": "build"
    }
  ]
}
```

## 验证安装
1. 打开Erlang文件应获得语法高亮
2. 使用Ctrl+Shift+P执行`Erlang LS: Start` 
3. 测试代码自动补全功能