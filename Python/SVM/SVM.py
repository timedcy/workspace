#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on May 3,2015
@author by Jason
"""

import numpy as np
import pandas as pd
import matplotlib as plt
import tushare as ts
from datetime import datetime
import pymysql
import math
from sklearn.decomposition import PCA, KernelPCA

# 打开数据库连接
conn = pymysql.connect(host='localhost', user='root', passwd='1735', db='winddata', port=3306, charset='utf8')
# 使用cursor()方法获取操作游标
cur = conn.cursor()

sql = "SELECT DISTINCT datetime FROM dailyprice WHERE datetime >= '2008-12-20'"
cur.execute(sql)
day = cur.fetchall()
n = len(day)
seq = list(range(1, n, 21))
dayseries = list(map(lambda x: day[x][0].strftime('%Y-%m-%d'), seq))
tradeday = dayseries[12:len(dayseries)]

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
    # 取出股票某个因子的数据
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
    # 获取各种因子的数据，并进行数据预处理
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
    west_netprofit_fy3 = getFactor(tradeday, stock, 'west_netprofit', 'west_netprofit_fy3')
    np_belongto_parcomsh = getFactor(tradeday, stock, 'west_netprofit', 'np_belongto_parcomsh')
    egib_s = list(map(lambda x, y: (x/y)**(1.0/3), west_netprofit_fy3, np_belongto_parcomsh))

    btop = getFactor(tradeday, stock, 'value', 'btop')       # 取出的数据有0，数据格式问题
    mlev = getFactor(tradeday, stock, 'new_leverage_mlev', 'mlev')
    dtoa = getFactor(tradeday, stock, 'new_leverage_dtoa', 'dtoa')
    blev = getFactor(tradeday, stock, 'new_leverage_blev', 'blev')
    stom = getFactor(tradeday, stock, 'new_liquidity', 'stom')
    # stoq stoa
    index = dayseries.index(tradeday)
    sto = list(map(lambda x: getFactor(x, stock, 'new_liquidity', 'stom'), dayseries[(index - 11):(index + 1)]))
    sto = pd.DataFrame(sto)
    stoq = []
    stoa = []

    def sumLog(data):
        return(math.log(math.e, sum(list(map(lambda x: math.exp(x), data)))/3))
    for i in range(np.shape(sto)[1]):
        stoq.append(sumLog(sto.ix[9:np.shape(sto)[0], i]))
        stoa.append(sumLog(sto.ix[:, i]))

    df = pd.DataFrame({'beta': beta,
                       'rstr': rstr,
                       'lncap': lncap,
                       'epibs': epibs,
                       'etop': etop,
                       'dastd': dastd,
                       'cmra': cmra,
                       'hsigma': hsigma,
                       'sgro': sgro,
                       'egro': egro,
                       'egib': egib,
                       'egib_s': egib_s,
                       'btop': btop,
                       'mlev': mlev,
                       'dtoa': dtoa,
                       'blev': blev,
                       'stom': stom,
                       'stoq': stoq,
                       'stoa': stoa}, index=stock)

    # 对缺失值进行处理
    df_count = df.count()
    col = list(map(lambda x: x <260, df_count))
    df.drop(df.columns[col], axis=1,inplace=True)
    # 对极值的处理，采用格拉布斯准则法，采用中位数来剔除异常值，并对数据进行标准化
    df_median = np.median(df, axis=0)
    df_std = np.std(df, axis=0)
    df_mean = np.mean(df, axis=0)
    for i in range(np.shape(df)[1]):
        for j in range(np.shape(df)[0]):
            if df.ix[j, i] > df_median[i] + 3*df_std[i]:
                df.ix[j, i] = df_median[i] + 3*df_std[i]
            if df.ix[j, i] < df_median[i] - 3*df_std[i]:
                df.ix[j, i] = df_median[i] - 3*df_std[i]
            if not np.isnan(df.ix[j, i]):
                df.ix[j, i] = (df.ix[j, i] - df_mean[i])/df_std[i]
    # 对部分缺失值赋予均值
    for i in range(np.shape(df)[1]):
        df.ix[np.isnan(df.ix[:,i]), i] = df_mean[i]
    return(df)

    #
    tradeday = tradeday[0]
    stock = list(map(lambda x: x[0], stock[tradeday]))
    df = getData(tradeday, stock)
    # 对数据应用Kpca方法降维
    kpca = KernelPCA(n_components=8, kernel="rbf", fit_inverse_transform=True, gamma=10)
    df_kpca = kpca.fit_transform(df)
    kpca.transform(df_new)

    # 对数据应用PCA方法降维
    pca=PCA(n_components=1)
    df_pca = pca.fit_transform(df)