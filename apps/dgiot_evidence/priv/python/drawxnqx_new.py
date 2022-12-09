import matplotlib.pyplot as plt
from pylab import mpl
import math
import numpy as np
from mpl_toolkits.axisartist.parasite_axes import HostAxes, ParasiteAxes
import time
import sys
import base64
import json

"""完成拟合曲线参数计算前相应变量的计算"""


def polynomial_fitting(data_x, data_y):
    size = len(data_x)
    i = 0
    sum_x = 0
    sum_sqare_x = 0
    sum_third_power_x = 0
    sum_four_power_x = 0
    sum_y = 0
    sum_xy = 0
    sum_sqare_xy = 0
    while i < size:
        sum_x += data_x[i]
        sum_y += data_y[i]
        sum_sqare_x += math.pow(data_x[i], 2)
        sum_third_power_x += math.pow(data_x[i], 3)
        sum_four_power_x += math.pow(data_x[i], 4)
        sum_xy += data_x[i] * data_y[i]
        sum_sqare_xy += math.pow(data_x[i], 2) * data_y[i]
        i += 1;
    return [[size, sum_x, sum_sqare_x, sum_y]
        , [sum_x, sum_sqare_x, sum_third_power_x, sum_xy]
        , [sum_sqare_x, sum_third_power_x, sum_four_power_x, sum_sqare_xy]]


"""完成拟合曲线参数的计算
 其中解方程的时候，利用高斯消元法计算相应的参数值
"""


def calculate_parameter(data):
    # i用来控制列元素，line是一行元素,j用来控制循环次数,datas用来存储局部变量。保存修改后的值
    i = 0;
    j = 0;
    line_size = len(data)

    # 将行列式变换为上三角行列式
    while j < line_size - 1:
        line = data[j]
        temp = line[j]
        templete = []
        for x in line:
            x = x / temp
            templete.append(x)
        data[j] = templete
        # flag标志应该进行消元的行数
        flag = j + 1
        while flag < line_size:
            templete1 = []
            temp1 = data[flag][j]
            i = 0
            for x1 in data[flag]:
                if x1 != 0:
                    x1 = x1 - (temp1 * templete[i])
                    templete1.append(x1)
                else:
                    templete1.append(0)
                i += 1
            data[flag] = templete1
            flag += 1
        j += 1

    # 求相应的参数值
    parameters = []
    i = line_size - 1
    # j标识减去元素个数
    # flag_rol标识除那一列
    flag_j = 0
    rol_size = len(data[0])
    flag_rol = rol_size - 2
    # 获得解的个数
    while i >= 0:
        operate_line = data[i]
        if i == line_size - 1:
            parameter = operate_line[rol_size - 1] / operate_line[flag_rol]
            parameters.append(parameter)
        else:
            flag_j = (rol_size - flag_rol - 2)
            temp2 = operate_line[rol_size - 1]
            # result_flag为访问已求出解的标志
            result_flag = 0
            while flag_j > 0:
                temp2 -= operate_line[flag_rol + flag_j] * parameters[result_flag]
                result_flag += 1
                flag_j -= 1
            parameter = temp2 / operate_line[flag_rol]
            parameters.append(parameter)
        flag_rol -= 1
        i -= 1
    return parameters


"""计算拟合曲线的值"""


def calculate(data_x, parameters):
    datay = []
    for x in data_x:
        datay.append(parameters[2] + parameters[1] * x + parameters[0] * x * x)
    return datay


"""完成函数的绘制"""


def draw(flow1, head1, headparameters, power1, powerparameters, effect, effectparameters, params):
    fm = math.ceil(max(flow1))
    hm = math.ceil(max(head1))
    hmin = math.floor(min(head1))
    pm = math.ceil(max(power1))
    pmin = math.floor(min(power1))
    nm = math.ceil(max(effect))
    nmin = math.floor(min(effect))

    # 图片大小 像素
    # 注意这里的宽度和高度的单位是英寸，1英寸=100像素，所以要除以100
    # plt.figure(figsize=(宽度 高度))
    fig = plt.figure(figsize=(11, 5.5))
    # HostAxes(figure,[ 左，下，宽，高 ]）
    host = HostAxes(fig, [0.2, 0.1, 0.65, 0.8])
    fig.add_axes(host)
    x = np.linspace(0, fm, 500)

    # 流量
    host.set_xlim(0, fm + 1)
    host.set_xlabel('流量(Q)(m3/h)')

    # 扬程
    par2 = ParasiteAxes(host, sharex=host)
    host.parasites.append(par2)
    par2.set_ylabel('扬程(H)(m)', color="blue")
    offset2 = (-45, 0)
    new_axisline2 = par2._grid_helper.new_fixed_axis
    par2.axis['right2'] = new_axisline2(loc='left', axes=par2, offset=offset2)
    y2 = headparameters[0] * x ** 2 + headparameters[1] * x + headparameters[2]
    # p3, = par2.plot(x, y2, label="HQ拟合曲线", color="blue")
    p3, = par2.plot(x, y2, color="blue")
    par2.scatter(flow1, head1, marker='o', c='b', label="扬程")
    par2.set_ylim(0, hm + 5)
    par2.axis['right2'].major_ticklabels.set_color(p3.get_color())  # 刻度值颜色
    # par2.axis['right2'].set_axisline_style('-|>', size=1.5)  # 轴的形状色

    # 功率
    par1 = ParasiteAxes(host, sharex=host)
    host.parasites.append(par1)
    par1.set_ylabel('功率(P)(kW)', color="red")
    offset1 = (-90, 0)
    new_axisline1 = par1._grid_helper.new_fixed_axis
    par1.axis['right'] = new_axisline1(loc='left', axes=par1, offset=offset1)
    y1 = powerparameters[0] * x ** 2 + powerparameters[1] * x + powerparameters[2]
    p2, = par1.plot(x, y1, color="red")
    par1.scatter(flow1, power1, marker='s', c='r', label="功率")
    par1.set_ylim(0, pm * 2)
    par1.axis['right'].major_ticklabels.set_color(p2.get_color())  # 刻度值颜色
    # par1.axis['right2'].set_axisline_style('-|>', size=1.5)  # 轴的形状色

    # 效率
    host.set_ylabel('效率(E)(%)', color="black")
    y = effectparameters[0] * x ** 2 + effectparameters[1] * x + effectparameters[2]
    p1, = host.plot(x, y, color="black")
    host.scatter(flow1, effect, marker='*', c='k', label="效率")
    host.set_ylim(0, nm + 5)
    host.axis['right'].set_visible(True)  # 刻度值

    #
    if 'dgiot_testing_equipment_flowSet' in params:
        flowSet = float(params['dgiot_testing_equipment_flowSet'])
        par2.axvline(flowSet, color='orange')

        effectpoint = round(np.interp(flowSet, x, y), 3)
        host.plot(flowSet, effectpoint, marker='*', color="orange")
        host.text(flowSet, effectpoint * 0.9,
                  str(effectpoint),
                  ha='center', color='k')

        headpoint = round(np.interp(flowSet, x, y2), 3)
        par2.plot(flowSet, headpoint, marker='o', c='orange')
        par2.text(flowSet, headpoint * 0.9,
                  str(headpoint),
                  ha='center', color='b')

        powerpoint = round(np.interp(flowSet, x, y1), 3)
        par1.plot(flowSet, powerpoint, marker='s', c='orange')
        par1.text(flowSet, powerpoint * 0.9,
                  str(powerpoint),
                  ha='center', color='r')

    # 解决使用matplotliblib画图的时候出现中文或者是负号无法显示的情况
    mpl.rcParams['font.sans-serif'] = ['SimHei']
    mpl.rcParams['axes.unicode_minus'] = False

    plt.title("泵性能曲线")

    # 自定义 图例顺序
    handles, labels = host.get_legend_handles_labels()
    handles = [handles[1], handles[2], handles[0]]
    labels = [labels[1], labels[2], labels[0]]
    # print(handles)
    # print(labels)

    plt.legend(handles, labels, loc=9, bbox_to_anchor=(1.1, 1.1), borderaxespad=0., fontsize=12)
    # 获取当前时间
    # localtime = time.strftime("%Y-%m-%d-%H-%M-%S", time.localtime())
    filepath = base64.b64decode(params['path']).decode("utf-8")
    filename = filepath + params['name']
    # print(filename)
    plt.savefig(filename)
    # plt.show()
    return (filename)


def find_close(arr, e):
    low = 0
    high = len(arr) - 1
    idx = -1

    while low <= high:
        mid = int((low + high) / 2)
        if e == arr[mid] or mid == low:
            idx = mid
            break
        elif e > arr[mid]:
            low = mid
        elif e < arr[mid]:
            high = mid

    if idx + 1 < len(arr) and abs(e - arr[idx]) > abs(e - arr[idx + 1]):
        idx += 1

    return arr[idx]


def main(argv):
    params = json.loads(base64.b64decode(argv).decode("utf-8"))
    # 流量
    flow = params['data']['flow']
    # 扬程
    head = params['data']['head']
    # 功率
    power = params['data']['power']
    # 效率
    effect = params['data']['effect']

    headdata = polynomial_fitting(flow, head)
    headparameters = calculate_parameter(headdata)

    powerdata = polynomial_fitting(flow, power)
    powerparameters = calculate_parameter(powerdata)

    effectdata = polynomial_fitting(flow, effect)
    effectparameters = calculate_parameter(effectdata)

    filename = draw(flow, head, headparameters, power, powerparameters, effect, effectparameters, params)
    after_base64 = base64.b64encode(filename.encode('utf-8')).decode('ascii')
    print(after_base64)


def exit():
    os._exit(0)


if __name__ == "__main__":
    main(
        "eyJkYXRhIjp7ImNvbnZlcnNpb25fZmxvdyI6WzI4LjI0MSwyNS4yNTksMjEuNTAzLDE4LjY0MSwxNS40MDksMTIuMTkzLDkuMjkxLDYuNzMxLDMuMjYsMC4wXSwiY29udmVyc2lvbl9oZWFkIjpbMi45NzYsNy41NzMsOS4xNTEsMTEuMTYyLDEzLjIzLDE1LjA4OCwxNi41MDYsMTcuNDY5LDE4LjQyMiwyMC4xOTddLCJjb252ZXJzaW9uX3Bvd2VyIjpbMi4wODUsMS45NjksMS42ODYsMS42ODYsMS41OCwxLjQ3NSwxLjM3LDEuMzcsMS4xNTksMS4xNTldLCJjdXJyZW50IjpbMy4zMzMsMy4zLDMuMiwzLjEsMy4wLDIuOSwyLjgsMi43LDIuNiwyLjVdLCJkZ2lvdGNvbGxlY3RmbGFnIjpbMC4wLDAuMCwwLjAsMC4wLDAuMCwwLjAsMC4wLDAuMCwwLjAsMC4wXSwiZG9uZ3ljIjpbMC43MzgsMC41OTEsMC40NTYsMC4zNDMsMC4yMzQsMC4xNDcsMC4wODUsMC4wNDUsMC4wMSwwLjBdLCJlZmZlY3QiOlsxMC45NzksMjYuNDU2LDMxLjc4OSwzMy42MTQsMzUuMTMsMzMuOTY3LDMwLjQ5MywyMy4zODEsMTQuMTEzLDAuMF0sImZsb3ciOlsyNi44OTIsMjQuMDUyLDIxLjEzMiwxOC4zMiwxNS4xNDMsMTEuOTgzLDkuMTMxLDYuNjE1LDMuMjA0LDAuMF0sImhlYWQiOlsyLjY5OSw2Ljg2Niw4LjgzOCwxMC43OCwxMi43NzgsMTQuNTcyLDE1Ljk0MiwxNi44NzIsMTcuNzkyLDE5LjUwNl0sIm1vdG9yc3BlZWQiOlsyOTkzLjAsMjk5My4wLDIuOWUzLDIuOWUzLDIuOWUzLDIuOWUzLDIuOWUzLDIuOWUzLDIuOWUzLDIuOWUzXSwicG93ZXIiOlsxLjgsMS43LDEuNiwxLjYsMS41LDEuNCwxLjMsMS4zLDEuMSwxLjFdLCJwcmVzc3VyZV9pbiI6WzAuMCwwLjAsMC4wLDAuMCwwLjAsMC4wLDAuMCwwLjAsMC4wLDAuMF0sInByZXNzdXJlX291dCI6WzAuMDAzMDQsMC4wNDUzNSwwLjA2NiwwLjA4NjE1LDAuMTA2OCwwLjEyNTI1LDAuMTM5MjgsMC4xNDg4LDAuMTU4MTUsMC4xNzUwNl0sInRpbWVzdGFtcCI6W10sInZvbCI6WzM5NC40LDM5NS4xMzMsMzk0LjYsMzkzLjI2NywzOTUuMiwzOTQuMywzOTQuOSwzOTUuMjMzLDM5NS43LDM5NC42NjddLCJ3YXRlcl90ZW1wZXJhdHVyZSI6WzE0LjUyLDE0LjU1LDE0LjU1LDE0LjU2LDE0LjU2LDE0LjU4LDE0LjU4LDE0LjU4LDE0LjU4LDE0LjZdfSwiZGdpb3RfdGVzdGluZ19lcXVpcG1lbnRfZmxvd1NldCI6MTAsIm5hbWUiOiJhYWFhYWFhYS5wbmciLCJwYXRoIjoiUkRwY2JYTjVjelkwWEdodmJXVmNjM1J2Ym1Wc2FYVmNaR2RwYjNSY1lYQndjMXhrWjJsdmRGOWxkbWxrWlc1alpWeHdjbWwyWEhCNWRHaHZibHc9In0=")
