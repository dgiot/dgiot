#!/usr/bin/env node
// install: yarn global add  rimraf   chalk   shelljs  inquirer
// use: node push.js or ./push.js
const rimraf = require('rimraf')
const logs = console.log
const path = require('path')
const chalk = require('chalk')
const shell = require('shelljs')
const inquirer = require('inquirer')
let commitType,
  commitMsg = ''
// function rimLog(file) {
//   const dirPath = path.resolve(__dirname, `${file}`)
//   rimraf(dirPath, (err) => {
//     if (err) {
//       throw err
//     } else logs(chalk.red(`${dirPath} File has been deleted`))
//   })
//   funCommitType()
// }
function funCommitType() {
  const promptList = [
    {
      type: 'list',
      message: '请选择commit类型:',
      name: 'env',
      choices: [
        { value: 'feat', name: 'feat: 新功能' },
        { value: 'fix', name: 'fix: 修复bug' },
        { value: 'style', name: 'style: 代码格式（空格、分号等）' },
        { value: 'refactor', name: 'refactor: 重构（非feat、非fix）' },
        { value: 'perf', name: 'perf: 提高性能' },
        { value: 'test', name: 'test: 添加缺少的测试' },
        { value: 'docs', name: 'docs: 文档修改' },
        {
          value: 'chore',
          name: 'chore: 杂务（对生成过程或辅助工具和库（如文档生成）的更改）',
        },
        { value: 'revert', name: 'revert: 还原到提交' },
        { value: 'WIP', name: 'WIP: 进行中的工作' },
        { value: 'workflow', name: 'workflow: 工作流相关文件修改' },
        { value: 'build', name: 'build: 构建过程或辅助工具的变动' },
        { value: 'ci', name: 'ci: 修改项目持续集成流程' },
        { value: 'release', name: 'release: 发布新版本' },
      ],
    },
  ]
  inquirer.prompt(promptList).then(({ env }) => {
    logs(chalk.blue('commit类型:' + env))
    commitType = env
    funCommitMessage()
  })
}
function funCommitMessage() {
  const promptList = [
    {
      type: 'input',
      message: '请输入提交message信息:',
      name: 'msg',
    },
  ]
  inquirer.prompt(promptList).then(({ msg }) => {
    commitMsg = msg
    logs(chalk.blue('message信息:' + msg))
    funShell()
  })
}
function funShell() {
  shell.exec('git pull')
  try {
    logs(chalk.green('changelog start'))
    shell.exec('rimraf CHANGELOG.md && conventional-changelog -p angular -i CHANGELOG.md -r 0 -s')
    logs(chalk.green('changelog end'))
  }catch{
    logs(chalk.red('changelog error'))
  }
  shell.exec('git add -A .')
  shell.exec(`git commit -m ${commitType}: &{commitMsg}`)
  shell.exec('git push')
}

funCommitType()

// rimLog('CHANGELOG.md')
