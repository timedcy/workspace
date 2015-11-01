# -*- coding:utf-8 -*-
'''
A股所有股票过去五年营业收入、归属母公司净利润复合增长率、未来一年和三年企业一致预期净利润增长率因子获取
'''
import numpy as np
import pandas as pd
import matplotlib as plt
from WindPy import *
from datetime import datetime
import pymysql

# 连接Wind API
w.start()

year = [2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015]
month = list(range(1, 13))
day = []
# 取出2010-2015年每月第一天
for i in range(len(year)):
    for j in range(len(month)):
        day.append(year[i] * 10000 + month[j] * 100 + 1)

oneday = timedelta(days = 1)
# 取出2010-2015年每月最后一天
yesterday = list(map(lambda x: int((datetime.strptime(str(x), '%Y%m%d') - oneday).strftime("%Y%m%d")), day))
yesterday = yesterday[0:130]

# 四月后取去年的数据，四月前取前年的数据
reportday = list(map(lambda x: (int(str(x)[0:4]) - 1) * 10000 + 1231 if int(str(x)[4:6]) >= 4 else (int(str(x)[0:4]) - 2) * 10000 + 1231, yesterday))

stock = {}
growth_cagr_tr = {}
growth_cagr_netprofit = {}
west_netprofit_YOY = {}
np_belongto_parcomsh = {}
west_netprofit_FY3 = {}

for item in yesterday:
    # 获取不同时间A股所有股票代码
    stock[item] = w.wset("SectorConstituent", u"date= %d ;sectorId=a001010100000000" % item)

# 获取过去五年营业收入和归属母公司净利润复合增长率
for i in range(len(year)):
    growth_cagr_tr[year[i]] = []
    growth_cagr_netprofit[year[i]] = []
    growth_cagr_tr[year[i]].append(w.wss(stock[yesterday[i*12]].Data[1], "growth_cagr_tr", u"year=%d;n=5" % year[i]))
    growth_cagr_netprofit[year[i]].append(w.wss(stock[yesterday[i*12]].Data[1], "growth_cagr_netprofit", u"year=%d;n=5" % year[i]))

# 未来一年和三年企业一致预期净利润增长率
for i in range(len(yesterday)): 
    west_netprofit_YOY[yesterday[i]] = []
    np_belongto_parcomsh[yesterday[i]] = []
    west_netprofit_FY3[yesterday[i]] = []
    west_netprofit_YOY[yesterday[i]].append(w.wss(stock[yesterday[i]].Data[1], "west_netprofit_YOY",u"tradeDate= %d " % yesterday[i]))
    np_belongto_parcomsh[yesterday[i]].append(w.wss(stock[yesterday[i]].Data[1], "np_belongto_parcomsh",u"rptDate=%d;rptType=1" % reportday[i]))
    west_netprofit_FY3[yesterday[i]].append(w.wss(stock[yesterday[i]].Data[1], "west_netprofit_FY3",u"tradeDate=%d" % yesterday[i]))

value1 = []
value2 = []
value3 = []
value4 = []
value5 = []

#先插入主键，然后更新数据库
index1 = []
index2 = []
for i in range(len(year)):
    for j in range(len(stock[yesterday[i*12]].Data[1])):
        index1.append((year[i], stock[yesterday[i*12]].Data[1][j]))
for i in range(len(yesterday)):
    for j in range(len(stock[yesterday[i]].Data[1])):
        index2.append((yesterday[i], stock[yesterday[i]].Data[1][j]))


for i in range(len(year)):
    for j in range(len(stock[yesterday[i*12]].Data[1])):
        value1.append((growth_cagr_tr[year[i]][0].Data[0][j], year[i], stock[yesterday[i*12]].Data[1][j]))

for i in range(len(year)):
    for j in range(len(stock[yesterday[i*12]].Data[1])):
        value2.append((growth_cagr_netprofit[year[i]][0].Data[0][j], year[i], stock[yesterday[i*12]].Data[1][j]))

for i in range(len(yesterday)):
    for j in range(len(stock[yesterday[i]].Data[1])):
        value3.append((west_netprofit_YOY[yesterday[i]][0].Data[0][j], yesterday[i], stock[yesterday[i]].Data[1][j]))

for i in range(len(yesterday)):
    for j in range(len(stock[yesterday[i]].Data[1])):
        value4.append((np_belongto_parcomsh[yesterday[i]][0].Data[0][j], yesterday[i], stock[yesterday[i]].Data[1][j]))

for i in range(len(yesterday)):
    for j in range(len(stock[yesterday[i]].Data[1])):
        value5.append((west_netprofit_FY3[yesterday[i]][0].Data[0][j], yesterday[i], stock[yesterday[i]].Data[1][j]))

value1 = pd.np.array(pd.DataFrame(value1).dropna()).tolist()
value2 = pd.np.array(pd.DataFrame(value2).dropna()).tolist()
value3 = pd.np.array(pd.DataFrame(value3).dropna()).tolist()
value4 = pd.np.array(pd.DataFrame(value4).dropna()).tolist()
value5 = pd.np.array(pd.DataFrame(value5).dropna()).tolist()

def delnan(value):
	for i in range(len(value)):
		value[i][0] = '{:.4f}'.format(value[i][0])
		value[i] = tuple(value[i])

# 打开数据库连接
con = pymysql.connect(host='localhost',user='root',passwd='123',db='winddata',port=3306,charset='utf8')

# 使用cursor()方法获取操作游标
cur = con.cursor()

sql1 = "INSERT INTO growth_cagr(year, secucode) VALUES (%s, %s)"
sql2 = "INSERT INTO west_netprofit(EndDate, secucode) VALUES (%s, %s)"

cur.executemany(sql1,index1)
cur.executemany(sql2,index2)
con.commit()

# SQL 插入语句
sql3 = "UPDATE growth_cagr SET growth_cagr_tr = %s WHERE year = %s and secucode = %s"
sql4 = "UPDATE growth_cagr SET growth_cagr_netprofit = %s WHERE year = %s and secucode = %s"
sql5 = "UPDATE west_netprofit SET west_netprofit_YOY = %s WHERE EndDate = %s and secucode = %s"
sql6 = "UPDATE west_netprofit SET np_belongto_parcomsh = %s WHERE EndDate = %s and secucode = %s"
sql7 = "UPDATE west_netprofit SET west_netprofit_FY3 = %s WHERE EndDate = %s and secucode = %s"

try:
    # 执行sql语句
    cur.executemany(sql3,value1)
    cur.executemany(sql4,value2)
    cur.executemany(sql5,value3)
    cur.executemany(sql6,value4)
    cur.executemany(sql7,value5)

    # 提交到数据库执行
    con.commit()
except:
    # Rollback in case there is any error
    con.rollback()


# 关闭数据库连接,释放资源
cur.close()
con.close()



w.wss("000002.SZ", "west_eps_FTM","tradeDate=20151016")