# See PyCharm help at https://www.jetbrains.com/help/pycharm/
from selenium.webdriver import Chrome
from selenium.webdriver.chrome.options import Options
import time
import random
from time import sleep, ctime

DRIVER_PATH = '/usr/bin/chromedriver'
def refresh(driver, num):
    for i in range(num):
        i += 1
        print(i)
        time.sleep(random.randint(1,3))
        try:
            driver.refresh() # 刷新方法 refresh
            print ('test pass: refresh successful')
        except Exception as e:
            print ("Exception found", format(e))
    print('end:%s' % ctime())

if __name__ == "__main__":
    # 设置浏览器
    options = Options()
    options.add_argument('--no-sandbox')
    options.add_argument('--headless')  # 无头参数
    options.add_argument('--disable-gpu')

    # 启动浏览器
    driver = Chrome(executable_path=DRIVER_PATH, options=options)
    #隐式等待30s
    driver.implicitly_wait(30)
    # 访问目标URL
    driver.get('https://prod.iotn2n.com')
    driver.find_element_by_xpath('/html/body/div[1]/div/div/div[2]/form/div[2]/div/div/input').send_keys("dgiot_dev")
    driver.find_element_by_xpath('/html/body/div[1]/div/div/div[2]/form/div[3]/div/div/input').send_keys("dgiot_dev")
    driver.find_element_by_xpath('/html/body/div[1]/div/div/div[2]/form/div[4]/div/div/div/button').click()
    time.sleep(1)

    cookies = driver.get_cookies()
    print(cookies[0]['value'])