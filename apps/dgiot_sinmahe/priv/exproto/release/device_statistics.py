# coding=utf8
import os,sys
import requests
import json
import base64
from dateutil.parser import parse
import datetime
import hashlib
import time
exec_count = 0
from threading import Timer


groupSession = ''
globalEnv = {}
s = requests.session()
host = '47.110.83.36'

# 打印时间函数
def printTime(inc):
    global exec_count
    if (exec_count < 1):
        t = Timer(inc, printTime, (inc,))
        t.start()
    else:
      os._exit(0)
    exec_count = exec_count + 1

def login():
    url = 'http://{}/iotapi/login'.format(host)
    headers = {'accept': 'application/json', "Content-Type": "text/plain"}
    body = {"username": 'sinmahedev', "password": '123456'}
    r = s.post(url, headers=headers, data=json.dumps(body))
    global globalEnv
    globalEnv = json.loads(r.content)  # 解码JSON对象
    global groupSession
    groupSession = r.json()['sessionToken']
    s.headers.update({"sessionToken": groupSession, 'Content-Type': 'application/json'})

def get_deviceid(productid,devaddr):
    data = 'Device' + productid + devaddr
    print(data)
    md5 = hashlib.md5(data.encode(encoding='UTF-8')).hexdigest()
    return  md5[0:10]

def time_data1(time_sj):                #传入单个时间比如'2019-8-01 00:00:00'，类型为str
    data_sj = time.strptime(time_sj,"%Y-%m-%d %H:%M:%S")       #定义格式
    time_int = int(time.mktime(data_sj))
    return time_int             #返回传入时间的时间戳，类型为int

def time_data2(time_sj):     #传入参数
    data_sj = time.localtime(time_sj)
    time_str = time.strftime("%Y-%m-%d %H:%M:%S",data_sj)            #时间戳转换正常时间
    return time_str       #返回日期，格式为str

def post(args,session,env):
    body = json.loads(base64.b64decode(args).decode("utf-8"))
    #print(body)
    url = 'http://{}:6020/rest/sql'.format(host)
    # curl -H 'Authorization: Basic <TOKEN>' -d '<SQL>' <ip>:<PORT>/rest/sql
    # TOKEN为{username}:{password}经过Base64编码之后的字符串，例如root:taosdata编码后为cm9vdDp0YW9zZGF0YQ==
    headers = {'Authorization': 'Basic cm9vdDp0YW9zZGF0YQ=='}
    deviceid = get_deviceid(body['productid'],body['devaddr'])
    timeint = body['timeint']+1
    # print(timeint)
    delta = datetime.timedelta(days=1)
    date1 = datetime.date.today()
    rows = []

    flag=True
    for i in range(0,timeint):
        strdate = date1.strftime('%Y-%m-%d')

        date2 = date1+delta
        strdate1 = date1.strftime('"%Y-%m-%d %H:%M:%S.000"')

        strdate2 = date2.strftime('"%Y-%m-%d %H:%M:%S.000"')
        data = "select createdat,servicetime, IdleTime, uptime, downtime from _" + body['productid'] + "._" + deviceid + "  where createdat > " + strdate1 + " and createdat <" + strdate2 + " ;"
        #print(data)
        date1 = date1 - delta
        r = requests.post(url, headers=headers,data=data)
        t = r.content # 读取返回的内容
        t = json.loads(t) # 解码JSON对象
        #print( json.JSONEncoder().encode(t))
        if t['data']==[]:
            item={ "日期": strdate, "服务时间": "0h" , "空闲时间": "0h" ,"上行时间": "0h", "下行时间":"0h"}
            print(item)
            rows.append(item)
            continue

        #print(json.JSONEncoder().encode(t))
        k=t['rows']
        servicetime=0
        idletime=0
        uptime=0
        downtime=0
        if flag==True:
            flag=False
            for j in range(0,t['rows']):
                try:
                    if j>=0 and j<k-1:
                        if t['data'][j+1][1]>=t['data'][j][1]:
                            servicetime+=(t['data'][j+1][1]-t['data'][j][1])/60
                            servicetime = round(servicetime, 3)
                        if t['data'][j+1][2]>=t['data'][j][2]:
                            idletime+=(t['data'][j+1][2]-t['data'][j][2])/60
                            idletime = round(idletime, 3)
                        if t['data'][j+1][3]>=t['data'][j][3]:
                            uptime+=(t['data'][j+1][3]-t['data'][j][3])/60
                            uptime = round(uptime, 3)
                        if t['data'][j+1][4]>=t['data'][j][4]:
                            downtime+=(t['data'][j+1][4]-t['data'][j][4])/60
                            downtime = round(downtime, 3)
                    else:
                        pass
                except:
                    pass
            item={ "日期": strdate, "服务时间": str(servicetime)+"h" , "空闲时间": str(idletime)+"h" ,"上行时间": str(uptime)+"h", "下行时间":str(downtime)+"h"}
            print(item)
            rows.append(item)
            pre = t['data'][0]
            # print(pre)
        else:
            for j in range(0,t['rows']):
                try:
                    if j>=0 and j<k-1:
                        if t['data'][j+1][1]>=t['data'][j][1]:
                            servicetime+=(t['data'][j+1][1]-t['data'][j][1])/60
                            servicetime = round(servicetime, 3)
                        if t['data'][j+1][2]>=t['data'][j][2]:
                            idletime+=(t['data'][j+1][2]-t['data'][j][2])/60
                            idletime = round(idletime, 3)
                        if t['data'][j+1][3]>=t['data'][j][3]:
                            uptime+=(t['data'][j+1][3]-t['data'][j][3])/60
                            uptime = round(uptime, 3)
                        if t['data'][j+1][4]>=t['data'][j][4]:
                            downtime+=(t['data'][j+1][4]-t['data'][j][4])/60
                            downtime = round(downtime, 3)
                    else:
                        if t['data'][j][1] <= pre[1]:
                            servicetime += (pre[1]-t['data'][j][1])/60
                            servicetime = round(servicetime, 3)
                        if t['data'][j][2] <= pre[2]:
                            idletime += (pre[2]-t['data'][j][2])/60
                            idletime = round(idletime, 3)
                        if t['data'][j][3] <=pre[3]:
                            uptime += (pre[3]-t['data'][j][3])/60
                            uptime = round(uptime, 3)
                        if t['data'][j][4]<=pre[4]:
                            downtime += (pre[4]-t['data'][j][4])/60
                            downtime = round(downtime, 3)
                except:
                    pass
            item={ "日期": strdate, "服务时间": str(servicetime)+"h", "空闲时间": str(idletime)+"h","上行时间": str(uptime)+"h","下行时间":str(downtime)+"h"}
            print(item)
            rows.append(item)
            pre = t['data'][0]
            # print(pre)

    deviceData = {
        "columns": ["日期", "小时数"],
        "rows": rows
    }

    data = json.JSONEncoder().encode(deviceData)
    enbody = base64.b64encode(data.encode('utf-8'))
    print(data)
    printTime(5)
    return enbody




def main():
    global groupSession
    global globalEnv
    login()
    body = {
        "productid": "389a16cda1",
        "devaddr": "BasicInformation_20083411",
        "timeint": 30
        #"devaddr": "BasicInformation_20100629"
    }

    data = json.JSONEncoder().encode(body)
    argvs = base64.b64encode(data.encode('utf-8'))
    print(argvs)
    data = json.JSONEncoder().encode(globalEnv)
    env = base64.b64encode(data.encode('utf-8'))
    print(env)
    return post(argvs, groupSession, env)

def exit():
    os._exit(0)

if __name__ == "__main__":
    main()