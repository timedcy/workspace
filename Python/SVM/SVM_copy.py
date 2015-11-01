#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Created on May 3,2015
@author by Jason
'''

import numpy as np
import pandas as pd
import matplotlib as plt
from WindPy import *
from datetime import datetime

# 连接Wind API
w.start()
tradeday = w.tdays("2010-01-01", "2015-10-12", "").Data[0]
tradeday = 
year = [2009, 2010, 2011, 2012, 2013, 2014, 2015]
month = list(range(1, 13))
day = []
for i in range(len(year)):
    for j in range(len(month)):
        day.append(year[i] * 10000 + month[j] * 100 + 1)
# 取出2010-2015年每月第一天
oneday = timedelta(days=1)
# 取出2010-2015年每月最后一天
yesterday =list(map(lambda x: int((datetime.strptime(str(x), '%Y%m%d') - oneday).strftime("%Y%m%d")), day))

tradeday = day[12:81]

# 取出某个时间点的报告期，前一年或者两年的最后一天
reportday = list(map(lambda x: (int(str(x)[0:4]) - 1) * 10000 + 1231 if int(str(x)[4:6]) >= 5
                else (int(str(x)[0:4]) - 2) * 10000 + 1231, yesterday))

stock = {}
data = {}

for item in tradeday:
    # 获取不同时间HS300成分股
    stock[item] = w.wset("IndexConstituent", u"date= %d ;windcode=000300.SH" % item)

for i in range(len(tradeday)):
    # 获取HS300成分股净利润_ttm,市值，企业价值，EBITDAvsEV
    data[tradeday[i]] = w.wss(stock[tradeday[i]].Data[1], "profit_ttm,ev,ev2,ev2_to_ebitda",u"tradeDate= %d" % yesterday[i+12]).Data

    # 获取HS300成分股总资产增长率，净利润增长率，资产总计，营业总收入，单季毛利率，单季净利率，单季ROE，单季ROA
    data[tradeday[i]].extend(w.wss(stock[tradeday[i]].Data[1], "growth_assets,growth_profit,tot_assets,tot_oper_rev,qfa_grossprofitmargin,qfa_netprofitmargin,qfa_roe_deducted,qfa_roa","rptDate=20140331;rptType=1;N=3").Data)

    # 三年的净利润和市值数据
    if int(str(day[i])[4:6]) >= 5:
        temp = w.wsd(stock[tradeday[i]].Data[1], "wgsd_net_inc", "ED-3Y", "2015-09-22", "rptType=1;currencyType=;Period=Y;Fill=Previous;PriceAdj=F").Data
        #对缺失数据的处理
        for i in range(np.shape(temp)[0]):
            temp1 = temp[i]
            for j in range(len(temp1)):
                if np.isnan(temp1[-(j+1)]):
                    temp1[-(j+1)] = temp1[-j]
        profit = map(lambda x:sum(x[0:3]),temp)
        temp = w.wsd(stock[tradeday[i]].Data[1], "ev", "ED-3Y", "2015-09-22", "Period=Y;Fill=Previous;PriceAdj=F").Data
        #对缺失数据的处理
        for i in range(np.shape(temp)[0]):
            temp1 = temp[i]
            for j in range(len(temp1)):
                if np.isnan(temp1[-(j+1)]):
                    temp1[-(j+1)] = temp1[-j]
        ev = map(lambda x:sum(x[0:3]),temp)
        ETP3 = map(lambda x,y:x/y,profit,ev)
    else:
        temp = w.wsd(stock[tradeday[i]].Data[1], "wgsd_net_inc", "ED-4Y", "2015-09-22", "rptType=1;currencyType=;Period=Y;Fill=Previous;PriceAdj=F")
        #对缺失数据的处理
        for i in range(np.shape(temp)[0]):
            temp1 = temp[i]
            for j in range(len(temp1)):
                if np.isnan(temp1[-(j+1)]):
                    temp1[-(j+1)] = temp1[-j]
        profit = map(lambda x:sum(x[0:3]),temp)
        temp = w.wsd(stock[tradeday[i]].Data[1], "ev", "ED-4Y", "2015-09-22", "Period=Y;Fill=Previous;PriceAdj=F")
        #对缺失数据的处理
        for i in range(np.shape(temp)[0]):
            temp1 = temp[i]
            for j in range(len(temp1)):
                if np.isnan(temp1[-(j+1)]):
                    temp1[-(j+1)] = temp1[-j]
        ev = map(lambda x:sum(x[0:3]),temp)
        ETP3 = map(lambda x,y:x/y,profit,ev)
    data[tradeday[i]].append(ETP3)
    
    # 获取前收盘价，算取1、3、6、12个月收益率
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "pre_close", "tradeDate=20150923;priceAdj=F;cycle=D"))
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "pre_close", "tradeDate=20150923;priceAdj=F;cycle=D"))
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "pre_close", "tradeDate=20150923;priceAdj=F;cycle=D"))
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "pre_close", "tradeDate=20150923;priceAdj=F;cycle=D"))

    # 换手率，days为时间
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "turn_nd", "days=-30;tradeDate=20150922"))
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "turn_nd", "days=-60;tradeDate=20150922"))
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "turn_nd", "days=-180;tradeDate=20150922"))
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "turn_nd", "days=-360;tradeDate=20150922"))

    # 区间最高价，区间最低价
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "high_per", "startDate=20150823;endDate=20150923;priceAdj=F"))
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "low_per", "startDate=20150823;endDate=20150923;priceAdj=F"))

    # 获取2个月波动率
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "stdevr", "startDate=20150823;endDate=20150923;period=2;returnType=1"))

    # 股票beta因子
    data[tradeday[i]].append(w.wss(stock[tradeday[i]].Data[1], "beta", "startDate=20150823;endDate=20150923;period=2;returnType=1;index=000001.SH"))

