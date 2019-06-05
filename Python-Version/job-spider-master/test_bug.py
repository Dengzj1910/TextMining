#!/usr/bin/python
# -*- coding: UTF-8 -*-

from bs4 import BeautifulSoup
import re
import requests

import sys

reload(sys)
sys.setdefaultencoding("utf-8")

import pymongo
import json
from bson import json_util

myclient = pymongo.MongoClient("mongodb://[username:password@]host1[:port1][,host2[:port2],...[,hostN[:portN]]][/[database][?options]]")
mydb = myclient["database"]
print mydb

mycollection = mydb["jobs"]

for doc in mycollection.find():
    print json.dumps(doc, sort_keys=True, indent=4, default=json_util.default)
