import json
import os
import time

# 导入工具类
from myscrapy.network_log_option import *

if __name__ == '__main__':
    # 使用工具类来获取options配置，而不是平时的webdriver.ChromeOptions()方法
    options = get_log_options()
    # 使用工具类来获取caps
    desired_capabilities = get_caps()
    # 这里也可以对options和caps加入其他的参数，比如代理参数等
    chrome = webdriver.Chrome(options=options, desired_capabilities=desired_capabilities)
    #chrome.get("https://www.baidu.com/")
    chrome.get("http://prod.iotn2n.com/#/login")
    chrome.find_element_by_xpath("/html/body/div[1]/div/div/div[2]/form/div[2]/div/div[1]/input").send_keys("dgiot_dev")
    chrome.find_element_by_xpath("/html/body/div[1]/div/div/div[2]/form/div[3]/div/div/input").send_keys("dgiot_dev")
    chrome.find_element_by_xpath("/html/body/div[1]/div/div/div[2]/form/div[4]/div/div/div/button").click()
    chrome.get("http://prod.iotn2n.com/#/dashboard/devicelist")
    chrome.find_element_by_xpath("/html/body/div[1]/div/div[1]/div[2]/div/div[3]/div/section/div/div[2]/div[1]/div[2]/div[2]/div/div[3]/div/button[2]/i").click()
    chrome.page_source
    chrome.quit();

    chrome.manage().window().maximize();
    chrome.maximize_window()
    # 用工具类来获取ajax请求日志
    logs = get_xhr_logs(chrome)
    for log in logs:
        print(log)
    chrome.quit()