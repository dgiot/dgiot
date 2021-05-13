#!/usr/bin/python
##
import os
import sys
import re
from bs4 import BeautifulSoup

##  get all test catalog
def get_test_all_catalog(name):
    CatalogList = []
    PackageNameList = []
    filepath = os.path.join(os.getcwd(), name)
    packagenames = os.listdir(name)
    for packagename in packagenames:
        tmp_path = os.path.join(filepath, packagename)
        if  os.path.isdir(tmp_path):
            CatalogList.append(tmp_path)
            PackageNameList.append(packagename)
    return CatalogList, PackageNameList

## Start Galaxy Engine
## Save index. d by extracting important information from index.html.
## extract index.html and deal with path information in index.html
def extract_core_content(path, filename, packagename):
    filepath = os.path.join(path, filename)
    if os.path.exists(filepath):
        fopen = open(filepath)
        filecontent = fopen.read()
        original_core_content = re.findall(r"<\s*body[^>]*>(.+?)<div class=\"copyright\">Copyright", filecontent, re.S)
        content = original_core_content[0]
        core_content = content.replace('<a href=\"', '<a href=\"'+packagename+"\\")
        ## change title
        core_content = core_content.replace('Test Results', packagename + ' Test Results')
        return core_content

def replace_href(path, filename, packagename):
    filepath = os.path.join(path, filename)
    if os.path.exists(filepath):
        fopen = open(filepath)
        filecontent = fopen.read()
        re_data = re.compile(r"<div class=\"copyright\">Copyright(.+?)<\s*/\s*html\s*>", re.S)
        original_core_content = re_data.sub('', filecontent)
        replacecontent = original_core_content.replace('<a href=\"', '<a href=\"'+packagename+"\\")
        replacecontent = replacecontent.replace('<link rel=\"stylesheet\" href=\"', '<link rel=\"stylesheet\" href=\"'+packagename+"\\")
        replacecontent = replacecontent.replace('<script type=\"text/javascript\" src=\"', '<script type=\"text/javascript\" src=\"'+packagename+"\\")   
        replacecontent = replacecontent.replace('Test Results', packagename + ' Test Results')
        ra_title = re.compile("<title>(.+?)</title>", re.S)
        replacecontent = ra_title.sub('<title> Test Result </title>', replacecontent)
        return replacecontent

# Splicing content
def splicing_content(name, filename):
    CoreContent = []
    index = 0,
    CatalogList, PackageNameList = get_test_all_catalog(name)
    for catalog, packageName in zip(CatalogList, PackageNameList):
        if(index == (0,)):
            Content = replace_href(catalog, filename, packageName)
            CoreContent.append(Content)
            index = 1
        else:
            Content = extract_core_content(catalog, filename, packageName)
            CoreContent.append(Content)
    str = ""
    con =  str.join('%s' % id for id in CoreContent)
    con = con + "</body></html>"
    return con
    
## process data statistics
def data_statistics(filestream):
    soup_string = BeautifulSoup(filestream, "html.parser")
    body_datas = soup_string.find_all("tbody")
    testpluginList = []
    testmoudleList = []
    testsuccessList = []
    testfailList = []
    testskipList = []
    allsuccessValue = allfailValue = allSkipValue = 0
    for body_data in body_datas:
        ## test moudle data
        # testmoudledata = body_data.find_all("a")
        testplugindata = body_data.find_all("tr")
        for testplugin in testplugindata:
            data = testplugin.find_all("td")
            testpluginname = data[8]
            con = testpluginname.find("a")['href']
            re_data = re.compile(r"\\ct_run(.+?).html", re.S)
            re_data = re_data.sub('', con)
            testpluginList.append(str(re_data))
            testmoudledata = testplugin.find_all("a")[0]
            testmoudleList.append(testmoudledata)
            successdata = data[3]
            testsuccessList.append(successdata)
            faildata = data[4]
            testfailList.append(faildata)
            skipdata = data[5]
            testskipList.append(skipdata)
    ## get all value         
    testallvalues = soup_string.find_all("tfoot")
    for testallvalue in testallvalues:
        data = testallvalue.find_all("td")
        allsuccessValue  +=  int(data[3].string)
        allfailValue += int(data[4].string)
        datas = str(data[5].string)
        re_skipdata = re.compile("(.+?)/", re.S)
        datas = str(re_skipdata.sub('', datas)).replace(")", "")
        allSkipValue += int(datas)
    tbody = ""
    table_head = "<center><h1>Data Outline</h1></center><br /><center><table id=\"SortableTable\"\><thead><tr><th>Test Name</th><th>Success</th><th>Failed</th><th>Skiped</th><th>Plugin Name</th></tr></thead>"
    for testplugin, testmoudle, testsuccess, testfail, testskip in zip(testpluginList, testmoudleList, testsuccessList, testfailList, testskipList):
        moudlebody = "<tbody><tr><td>{}</td>".format(testmoudle)
        successbody = "{}".format(testsuccess)
        failbody = "{}".format(testfail)
        skipbody = "{}".format(testskip)
        pluginbody = "<td>{}</td></tr></tbody>".format(testplugin)
        tbody += str(moudlebody) + str(successbody) +str(failbody) + str(skipbody) + str(pluginbody)
    totalbody = "<tfoot><tr><td><b>Total</b></td><td>{}</td><td><font color=\"red\">{}</font></td><td><font color=\"green\">{}</font></td><td>&nbsp;</td></tr></tfoot></table>".format(allsuccessValue, allfailValue, allSkipValue)
    outline_head = "<center><h1>Executing Plugin Name</h1></center><br /><center><table id=\"SortableTable\"\><thead><tr><th>All Test Plugin Name</th><th>Success extract Log plugin name</th><th>Empty plugin name</th></tr></thead>"
    catalogList, packageNameList = get_test_all_catalog("logs")
    successPluginData = ""
    failedPluginData = ""
    for catalog, packagename in zip(catalogList, packageNameList):
        successPlugin, failedPlugin = outline_value(catalog, packagename)
        successPluginData += successPlugin + " "
        failedPluginData += failedPlugin +" "
    outline_body_value =  "<tbody><tfoot><tr><td>{}</td><td>{}</td><td><font color=\"red\">{}<font></td></tr></tfoot></tbody></table>".format(packageNameList, successPluginData, failedPluginData)
    data_outline_html = outline_head + outline_body_value + table_head + tbody + totalbody

    return  data_outline_html, allfailValue

def outline_value(path, pluginName):
    filepath = os.path.join(path, "index.html")
    successPlugin = ""
    failedPlugin = ""
    if os.path.exists(filepath):
        successPlugin = pluginName
    else:
        failedPlugin = pluginName
    return successPlugin, failedPlugin

def produceHtml():
    htmldata = splicing_content("logs", "index.html")
    data_outline_html, allfailValue = data_statistics(htmldata)
    headdatamatch = re.match(r"<!DOCTYPE(.+?)<\s*body[^>]*>", htmldata, re.S)
    headdata = ""
    if headdatamatch:
        headdata = headdatamatch.group()
    # re_data = re.compile(r"<!DOCTYPE (.+?)<\s*body[^>]*>", re.S)
    replacedata = headdata + data_outline_html
    htmldata = htmldata.replace(headdata, replacedata)
    savepath = os.path.join(os.getcwd(), "logs")
    savepath = os.path.join(savepath, "index.html")
    if os.path.exists(savepath):
        os.remove(savepath)
    f = open(savepath, 'w')
    f.write(htmldata)
    f.close
   
    ## send exit message when failed
    if allfailValue >0 :
        exit(1)
# htmldata = splicing_content("logs", "index.html")
# data_statistics(htmldata)
produceHtml()