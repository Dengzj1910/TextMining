#!/usr/bin/python
# -*- coding: UTF-8 -*-

from bs4 import BeautifulSoup
import re
import requests

import sys
import json

reload(sys)
sys.setdefaultencoding("utf-8")

url_website = "https://www.jobWebsite.com"

hea = {
    'User-Agent':
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36'}
search = {
    'typedKeyword': 'Software+Engineer',
    'sc.keyword': 'Software+Engineer',
    'locT': 'S',
    'locId': '2280',
    'locKeyword': 'California',
    'includeNoSalaryJobs': 'false',
    'jobType': 'fulltime',
    'fromAge': '-1',
    'minSalary': '10',
    'radius': '-1',
    'cityId': '-1',
    'minRating': '3.0',
    'industryId': '-1',
    'companyId': '-1',
    'employerSizes': '5',  # >5000
    'remoteWorkType': '0'
}
website_search_url = url_website + '/Job/jobs.htm?'
len_dict = len(search)
i = 0
for key, value in search.iteritems():
    website_search_url = website_search_url + key + "=" + value
    if i != len_dict:
        website_search_url += "&"
print u"website搜索页首页是: ", website_search_url

html = requests.get(website_search_url, headers=hea)
html.encoding = 'utf-8'

soup = BeautifulSoup(html.text, "html.parser")
links = soup.find_all('a', href=re.compile(r"-jobs-SRCH"))

second_page = ""
for item in links:
    if item.get_text() == '2':
        second_page = url_website + item['href']

url_list = []
for i in range(0, 20):
    if i == 0:
        url_list.append(second_page.replace("_IP2.htm", ".htm"))
    else:
        url_list.append(second_page.replace("_IP2.htm", "_IP" + str(i + 1) + ".htm"))

print u"前" + str(len(url_list)) + u"个分页链接: "
for url in url_list:
    print url

print
print

job_detail_list = []
url_count = 1
for url in url_list:
    if url_count % 2 == 0:
        print u"正在解析分页数据", str(url_count) + "/" + str(len(url_list))

    url_count += 1
    html = requests.get(url, headers=hea)
    html.encoding = 'utf-8'
    soup = BeautifulSoup(html.text, "html.parser")

    table_div = soup.find('div', {"class": "dataTableContainer"})
    for idx, row in enumerate(table_div.find_all('tr')):
        if idx != 0:
            cells = row.find_all('td')
            sl = cells[3].contents[0].contents[0]
            job_detail = {'job_title': cells[0].text,
                          "company": cells[1].contents[0].contents[0],
                          "location": cells[2].contents[0].contents[0],
                          "salary": cells[3].contents[0].contents[0],
                          'job_url': url_website + cells[0].contents[0].attrs['href'],
                          'plain_texts': "",
                      'success_parsed': False}
            job_detail_list.append(job_detail)

print u"共搜索到" + str(len(job_detail_list)) + u"个职位, 准备逐个分析"

success_get = 1
for job_details in job_detail_list:
    if success_get % 5 == 0:
        print u"正在请求职位描述数据原始html", str(success_get) + "/" + str(len(job_detail_list))
    try:
        html = requests.get(job_details['job_url'], headers=hea)
        html.encoding = 'utf-8'
        job_details['job_html'] = html.text
        success_get = success_get + 1
    except TypeError as e:
        print e
        print "html_url_html\'s job url:", job_details['job_url']

print u"请求分析职位描述数据原始html结束，成功比例: ", str(success_get - 1) + "/" + str(len(job_detail_list))

success_parse = 1
for job_details in job_detail_list:
    if success_parse % 10 == 0:
        print u"正在分析职位描述数据原始html", str(success_parse) + "/" + str(len(job_detail_list))

    html = job_details['job_html']
    try:
        soup = BeautifulSoup(html, "html.parser")
        paragraphs = soup.find('div', {"class": "jobDescriptionContent"})
        plain_texts = ""
        for paragraph in paragraphs:
            plain_text1 = BeautifulSoup(str(paragraph), "lxml").get_text()
            plain_texts = plain_texts + plain_text1

        job_details['plain_texts'] = plain_texts
        job_details['success_parsed'] = True
        success_parse = success_parse + 1
    except TypeError as e:
        print e
        print "error html_url_html\'s job html:", html

print u"职位描述数据原始html分析结束，成功比率:", str(success_parse - 1) + "/" + str(len(job_detail_list))

myClassJson = json.dumps(job_detail_list)

import pymongo

print u"正在连接mongo数据库: website_job_data"

#
myclient = pymongo.MongoClient("mongodb://[username:password@]host1[:port1][,host2[:port2],...[,hostN[:portN]]][/[database][?options]]")
mydb = myclient["website_job_data"]
print u"mongo数据库链接成功"

mycollection = mydb["jobs"]
print u"即将删除所有jobs表中内容"
x = mycollection.delete_many({})
print str(x.deleted_count), u"数据已成功删除"

print u"准备写入" + str(success_parse - 1), u"条职位描述数据"
for job_detail in job_detail_list:
    if job_detail['success_parsed']:
        mycollection.insert_one(job_detail)
print u"数据写入完毕"
