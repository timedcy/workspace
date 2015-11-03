#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Created on May 3,2015
@author by Jason
'''

import numpy as np
import pandas as pd
import matplotlib as plt
import tushare as ts
from datetime import datetime
import pymysql

# 打开数据库连接
conn = pymysql.connect(host='localhost', user='root', passwd='1735', db='winddata', port=3306, charset='utf8')
# 使用cursor()方法获取操作游标
cur = conn.cursor()

sql = "SELECT DISTINCT datetime FROM dailyprice WHERE datetime >= '2010-01-01'"
cur.execute(sql)
day = cur.fetchall()
n = len(day)
seq = list(range(1, n, 21))
tradeday = list(map(lambda x: day[x][0].strftime('%Y-%m-%d'), seq))

stock = {}
data = {}

for item in tradeday:
    # 取出数据库中某交易日前的有hs300成分股和权重的日期
    sql = "SELECT DISTINCT datetime FROM hs300weight WHERE datetime <= %s ORDER BY datetime DESC"
    cur.execute(sql, item)
    hs300_day = cur.fetchall()[0][0].strftime('%Y-%m-%d')
    # 获取具体日期下HS300成分股
    sql = "SELECT trade_code, weight FROM hs300weight WHERE datetime = %s"
    cur.execute(sql, hs300_day)
    stock[item] = list(map(lambda x: (x[0][0:6], x[1]), list(cur.fetchall())))

def getFactor(tradeday, stock, table, factor):
    # 取出股票因子的数据
    sql = "SELECT DISTINCT TradingDay FROM %s WHERE TradingDay <= '%s' ORDER BY TradingDay DESC" % (table, tradeday)
    cur.execute(sql)
    tradeday = cur.fetchall()[0][0].strftime('%Y-%m-%d')

    sql = "SELECT %s FROM %s WHERE TradingDay = '%s' and SecuCode = %s" % (factor, table, tradeday, '%s')
    result = []
    for item in stock:
        cur.execute(sql, item)
        temp = cur.fetchone()
        if temp is None:
            result.append(np.nan)
        elif temp[0] is None:
            result.append(np.nan)
        else:
            result.append(temp[0])
    return(result)



def getData(tradeday, stock):
    # 获取各种因子的数据
    beta = getFactor(tradeday, stock, 'new_beta', 'beta')
    rstr = getFactor(tradeday, stock, 'rstr_fuquan', 'rstr')
    lncap = getFactor(tradeday, stock, 'new_size', 'size')
    epibs = getFactor(tradeday, stock, 'west_eps', 'west_eps_ftm')
    etop = getFactor(tradeday, stock, 'newey_ep', 'etop')
    dastd = getFactor(tradeday, stock, 'dastd_fuquan', 'dastd')
    cmra = getFactor(tradeday, stock, 'cmra_fuquan', 'cmra')
    hsigma = getFactor(tradeday, stock, 'new_hsigma', 'hsigma')
    # sgro egro
    if(int(tradeday[5:7])>=5):
        Year = tradeday[0:4]
    else:
        Year = str(int(tradeday[0:4])-1)

    sql1 = "SELECT %s FROM %s WHERE Year = '%s' and SecuCode = %s" % ('growth_cagr_tr', 'growth_cagr', Year, '%s')
    sql2 = "SELECT %s FROM %s WHERE Year = '%s' and SecuCode = %s" % ('growth_cagr_netprofit', 'growth_cagr', Year, '%s')
    sgro = []
    egro = []
    for item in stock:
        cur.execute(sql1, item)
        temp1 = cur.fetchone()
        if temp1 is None:
            sgro.append(temp1)
        else:
            sgro.append(temp1[0])
        cur.execute(sql2, item)
        temp2 = cur.fetchone()
        if temp2 is None:
            egro.append(temp2)
        else:
            egro.append(temp2[0])
    # egib egib_s
    egib = getFactor(tradeday, stock, 'west_netprofit', 'west_netprofit_yoy')
    egib_s =

    btop = getFactor(tradeday, stock, 'value', 'btop')       #取出的数据有0，数据格式问题
    mlev = getFactor(tradeday, stock, 'new_leverage_mlev', 'mlev')
    dtoa = getFactor(tradeday, stock, 'new_leverage_dtoa', 'dtoa')
    blev = getFactor(tradeday, stock, 'new_leverage_blev', 'blev')
    stom = getFactor(tradeday, stock, 'new_liquidity', 'stom')
    # stoq stoa







    #
    stock = list(map(lambda x: x[0], stock[tradeday]))
        