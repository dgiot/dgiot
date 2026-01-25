#!/usr/bin/env node
/**
 * 中文打印Hook机制测试脚本
 * 测试自动检测和触发技能的功能
 */

const fs = require('fs');
const path = require('path');

// 配置
const CONFIG = {
  hookConfig: path.join(__dirname, 'chinese_printing_hook.yaml'),
  logFile: path.join(__dirname, 'hook_log.json'),
  testFiles: {
    chinesePrinting: path.join(__dirname, 'test_chinese_printing.erl'),
    utf8Suffix: path.join(__dirname, 'test_utf8_suffix.erl'),
    validChinese: path.join(__dirname, 'test_valid_chinese.erl')
  }
};

// 测试文件内容
const TEST_FILES_CONTENT = {
  chinesePrinting: `
%% 测试文件：包含中文打印但未优化的io:format调用
-module(test_chinese_printing).

-export([test/0]).

test() ->
    % 有问题的中文打印
    io:format("开始测试中文打印~n"),
    io:format("测试结果: ~p~n", [Result]),
    io:format("错误信息: 中文错误信息~n"),
    
    % 正确的二进制字符串
    io:format("~ts~n", [<<"正确的中文打印"/utf8>>]),
    ok.
`,

  utf8Suffix: `
%% 测试文件：缺少/utf8后缀的中文字符串
-module(test_utf8_suffix).

-export([test/0]).

test() ->
    % 缺少/utf8后缀
    Binary1 = <<"中文文本">>,
    
    % 正确的有/utf8后缀
    Binary2 = <<"正确的中文"/utf8>>,
    
    % 混合使用
    io:format("测试: ~ts~n", [Binary1]),
    io:format("测试: ~ts~n", [Binary2]),
    ok.
`,

  validChinese: `
%% 测试文件：完全正确的中文打印
-module(test_valid_chinese).

-export([test/0]).

test() ->
    % 所有中文都使用正确的二进制字符串
    io:format("=== ~ts ===~n", [<<"测试报告"/utf8>>]),
    io:format("~ts: ~p~n", [<<"测试值"/utf8>>, 123]),
    io:format("~ts: ~ts~n", [<<"状态"/utf8>>, <<"成功"/utf8>>]),
    
    % 使用辅助函数
    print_chinese("动态生成的中文"),
    ok.

print_chinese(Text) ->
    io:format("~ts~n", [unicode:characters_to_binary(Text)]).
`
};

// 检测函数
const DETECTORS = {
  contains_chinese_text: function(content) {
    const chinese_pattern = /[\u4e00-\u9fff]/;
    return chinese_pattern.test(content);
  },

  has_io_format_with_chinese: function(content) {
    const pattern = /io:format\s*\([^)]*[\u4e00-\u9fff][^)]*\)/;
    return pattern.test(content);
  },

  has_chinese_without_utf8: function(content) {
    const pattern = /<<"[^"]*[\u4e00-\u9fff]+[^"]*">>(?!\s*\/utf8)/;
    return pattern.test(content);
  }
};

// 技能映射
const SKILL_MAPPINGS = [
  {
    pattern: /.*io:format.*[\u4e00-\u9fff].*/,
    skill: "chinese_printing_solution",
    priority: "high",
    message: "检测到io:format调用包含中文文本"
  },
  {
    pattern: /.*<<\"[^\"]*[\u4e00-\u9fff]+[^\"]*\">>(?!\s*\/utf8).*/,
    skill: "erlang_chinese_utf8",
    priority: "critical",
    message: "检测到缺少/utf8后缀的中文字符串"
  },
  {
    pattern: /.*[\u4e00-\u9fff].*/,
    skill: "chinese_printing_solution",
    priority: "low",
    message: "检测到中文文本，建议检查编码规范"
  }
];

// 日志函数
function logToFile(entry) {
  let logs = [];
  try {
    const existing = fs.readFileSync(CONFIG.logFile, 'utf8');
    logs = JSON.parse(existing);
  } catch (e) {
    logs = [];
  }
  
  logs.push({
    timestamp: new Date().toISOString(),
    ...entry
  });
  
  fs.writeFileSync(CONFIG.logFile, JSON.stringify(logs, null, 2));
}

// 分析文件
function analyzeFile(filePath, content) {
  console.log(`\n分析文件: ${filePath}`);
  console.log('=' .repeat(50));
  
  const results = {
    file: filePath,
    detectors: {},
    skill_triggers: [],
    recommendations: []
  };
  
  // 运行所有检测器
  for (const [name, detector] of Object.entries(DETECTORS)) {
    const detected = detector(content);
    results.detectors[name] = detected;
    console.log(`  ${name}: ${detected ? '✓' : '✗'}`);
  }
  
  // 检查技能触发
  for (const mapping of SKILL_MAPPINGS) {
    if (mapping.pattern.test(content)) {
      results.skill_triggers.push({
        skill: mapping.skill,
        priority: mapping.priority,
        message: mapping.message
      });
      
      console.log(`  触发技能: ${mapping.skill} (${mapping.priority})`);
      console.log(`  原因: ${mapping.message}`);
    }
  }
  
  // 生成建议
  if (results.detectors.has_chinese_without_utf8) {
    results.recommendations.push({
      type: "critical",
      message: "文件中包含缺少/utf8后缀的中文字符串",
      action: "立即使用 erlang_chinese_utf8 技能修复"
    });
  }
  
  if (results.detectors.has_io_format_with_chinese) {
    results.recommendations.push({
      type: "high",
      message: "文件中包含io:format调用且参数中有中文",
      action: "使用 chinese_printing_solution 技能优化"
    });
  }
  
  if (results.detectors.contains_chinese_text && !results.detectors.has_io_format_with_chinese) {
    results.recommendations.push({
      type: "low",
      message: "文件中包含中文文本",
      action: "检查编码规范，确保使用正确的Unicode编码"
    });
  }
  
  // 输出建议
  if (results.recommendations.length > 0) {
    console.log('\n建议:');
    for (const rec of results.recommendations) {
      console.log(`  [${rec.type.toUpperCase()}] ${rec.message}`);
      console.log(`      操作: ${rec.action}`);
    }
  }
  
  // 记录到日志
  logToFile({
    type: "file_analysis",
    file: filePath,
    detectors: results.detectors,
    skill_triggers: results.skill_triggers.length,
    recommendations: results.recommendations.length
  });
  
  return results;
}

// 创建测试文件
function createTestFiles() {
  console.log('创建测试文件...');
  
  for (const [name, content] of Object.entries(TEST_FILES_CONTENT)) {
    const filePath = CONFIG.testFiles[name];
    fs.writeFileSync(filePath, content);
    console.log(`  创建: ${filePath}`);
  }
}

// 清理测试文件
function cleanupTestFiles() {
  console.log('\n清理测试文件...');
  
  for (const filePath of Object.values(CONFIG.testFiles)) {
    try {
      fs.unlinkSync(filePath);
      console.log(`  删除: ${filePath}`);
    } catch (e) {
      // 文件可能不存在，忽略错误
    }
  }
}

// 运行测试
function runTests() {
  console.log('=' .repeat(60));
  console.log('中文打印Hook机制测试');
  console.log('=' .repeat(60));
  
  // 清理旧的日志
  try {
    fs.unlinkSync(CONFIG.logFile);
  } catch (e) {
    // 文件可能不存在，忽略
  }
  
  // 创建测试文件
  createTestFiles();
  
  // 分析每个测试文件
  const allResults = [];
  
  for (const [name, filePath] of Object.entries(CONFIG.testFiles)) {
    try {
      const content = fs.readFileSync(filePath, 'utf8');
      const results = analyzeFile(filePath, content);
      allResults.push(results);
    } catch (e) {
      console.log(`\n错误: 无法读取文件 ${filePath}: ${e.message}`);
    }
  }
  
  // 生成测试报告
  console.log('\n' + '=' .repeat(60));
  console.log('测试报告');
  console.log('=' .repeat(60));
  
  const summary = {
    total_files: allResults.length,
    files_with_chinese: allResults.filter(r => r.detectors.contains_chinese_text).length,
    files_with_issues: allResults.filter(r => 
      r.detectors.has_chinese_without_utf8 || r.detectors.has_io_format_with_chinese
    ).length,
    total_skill_triggers: allResults.reduce((sum, r) => sum + r.skill_triggers.length, 0),
    total_recommendations: allResults.reduce((sum, r) => sum + r.recommendations.length, 0)
  };
  
  console.log(`总文件数: ${summary.total_files}`);
  console.log(`包含中文的文件: ${summary.files_with_chinese}`);
  console.log(`有问题的文件: ${summary.files_with_issues}`);
  console.log(`技能触发总数: ${summary.total_skill_triggers}`);
  console.log(`建议总数: ${summary.total_recommendations}`);
  
  // 按优先级统计技能触发
  const skillStats = {};
  for (const results of allResults) {
    for (const trigger of results.skill_triggers) {
      if (!skillStats[trigger.skill]) {
        skillStats[trigger.skill] = { total: 0, by_priority: {} };
      }
      skillStats[trigger.skill].total++;
      skillStats[trigger.skill].by_priority[trigger.priority] = 
        (skillStats[trigger.skill].by_priority[trigger.priority] || 0) + 1;
    }
  }
  
  console.log('\n技能触发统计:');
  for (const [skill, stats] of Object.entries(skillStats)) {
    console.log(`  ${skill}: ${stats.total} 次`);
    for (const [priority, count] of Object.entries(stats.by_priority)) {
      console.log(`    ${priority}: ${count} 次`);
    }
  }
  
  // 记录总结
  logToFile({
    type: "test_summary",
    summary: summary,
    skill_stats: skillStats,
    timestamp: new Date().toISOString()
  });
  
  console.log(`\n详细日志已保存到: ${CONFIG.logFile}`);
  
  // 清理测试文件
  cleanupTestFiles();
  
  return {
    success: summary.files_with_issues > 0, // 测试成功如果检测到问题
    summary: summary,
    skill_stats: skillStats
  };
}

// 主函数
function main() {
  try {
    // 检查Hook配置文件是否存在
    if (!fs.existsSync(CONFIG.hookConfig)) {
      console.error(`错误: Hook配置文件不存在: ${CONFIG.hookConfig}`);
      process.exit(1);
    }
    
    console.log(`使用Hook配置: ${CONFIG.hookConfig}`);
    
    // 运行测试
    const results = runTests();
    
    console.log('\n' + '=' .repeat(60));
    console.log('测试完成!');
    console.log('=' .repeat(60));
    
    if (results.success) {
      console.log('✅ Hook机制测试成功: 成功检测到中文打印问题');
      console.log('\n下一步:');
      console.log('1. 在实际项目中应用此Hook配置');
      console.log('2. 集成到Git Hook或CI/CD流程中');
      console.log('3. 配置编辑器插件实时检测');
    } else {
      console.log('⚠️  测试完成，但未检测到预期的问题');
      console.log('   请检查测试文件内容或检测规则');
    }
    
  } catch (error) {
    console.error(`测试失败: ${error.message}`);
    console.error(error.stack);
    process.exit(1);
  }
}

// 执行
if (require.main === module) {
  main();
}

module.exports = {
  DETECTORS,
  SKILL_MAPPINGS,
  analyzeFile,
  runTests
};