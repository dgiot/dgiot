// 测试配置 - 无硬编码
module.exports = {
    getProjectRoot: function() {
        // 从环境变量获取
        if (process.env.DGIOT_PROJECT_ROOT) {
            return process.env.DGIOT_PROJECT_ROOT;
        }
        
        // 从当前目录计算
        const path = require('path');
        const scriptDir = __dirname;
        return path.resolve(scriptDir, '../../..');
    },
    
    getHooksDir: function() {
        const path = require('path');
        const projectRoot = this.getProjectRoot();
        return path.join(projectRoot, '.cline/hooks');
    },
    
    getConfig: function() {
        return {
            hookConfig: path.join(this.getHooksDir(), 'chinese_printing_hook.yaml'),
            logFile: path.join(this.getHooksDir(), 'hook_log.json'),
            testFiles: {
                chinesePrinting: path.join(this.getHooksDir(), 'test_chinese_printing.erl'),
                utf8Suffix: path.join(this.getHooksDir(), 'test_utf8_suffix.erl'),
                validChinese: path.join(this.getHooksDir(), 'test_valid_chinese.erl')
            }
        };
    }
};
