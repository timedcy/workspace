#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import matplotlib as plt
from WindPy import *
from datetime import datetime
import pymysql

# 打开数据库连接
con = pymysql.connect(host='localhost', user='root', passwd='1735', db='winddata', port=3306, charset='utf8')
# 使用cursor()方法获取操作游标
cur = con.cursor()

# 连接Wind API
w.start()
#从Wind取出所有交易日
day = w.tdays("2010-01-01", "2015-10-12", "").Data[0]
#每21个交易日进行换仓，获取所有的交易日
seq = list(range(0, len(day), 21))
tradeday = list(map(lambda x: day[x].strftime("%Y%m%d"), seq))

stock = {}
data = {}

for item in tradeday:
    # 获取不同时间HS300成分股
    stock[item] = w.wset("IndexConstituent", u"date= %d ;windcode=000300.SH" % item)


sql = "SELECT DISTINCT SecuCode FROM new_size"

sql = "SELECT * from new_beta_copy where SecuCode=%s"
