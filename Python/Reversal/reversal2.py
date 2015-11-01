#-*- coding:utf-8 -*-
'''
Created on Aug 7,2015
@author by Jason
'''

import xlrd
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from WindPy import *
from datetime import *

global nrows,ncols

def get_Data(excel):
	global nrows,ncols
	#读取excel数据，获取开盘价和收盘价数据
	xls = xlrd.open_workbook(excel)
	table = xls.sheets()[0]
	#取出行列数
	nrows = table.nrows
	ncols = table.ncols
	#取出所有股票代码
	num = table.row_values(1)
	while '' in num:
		num.remove('')

	N = 300
	rows = range(4,nrows)
	cols = range(1,ncols,4)
	opens = np.zeros((nrows-4,N))
	closes = np.zeros((nrows-4,N))

	for i in range(nrows-4):
		for j in range(N):
			if table.cell(rows[i],cols[j]).value:
				opens[i,j] = float(table.cell(rows[i],cols[j]).value)
			if table.cell(rows[i],cols[j]+1).value:
				closes[i,j] = float(table.cell(rows[i],cols[j]+1).value)
	return opens, closes

def getRet(opens, closes, n):
	global nrows,ncols
	#按照收盘价和开盘价变化幅度排序
	ratio = (closes - opens)/opens
	index = np.argsort(ratio, axis = 1)[:,0:n]

	total = np.zeros(nrows-4)
	total[0] = 1000000
	ret = np.zeros(nrows-5)
	amount = np.zeros((nrows-5,n))

	#选取前n支股票
	for i in range(nrows-5):
		amount[i] = (total[i]/n)/closes[i,index[i,:]]
		total[i+1] = sum(amount[i,:]*opens[i+1,index[i,:]])
		ret[i] = (total[i+1]-total[i])/total[i]
	return pd.Series(ret)

def maxDrawdown(ret):
	for i in range(len(ret)):
		if ret[i] is np.nan:
			ret[i] = 0
	cum = ret.cumsum()
	drawdown = cum - cum.cummax()
	return min(drawdown)

def annualizedrReturn(ret):
	return ret.mean()*250

def annualizedSharpeRatio(ret):
	return ret.mean()/ret.std()*np.sqrt(250)
	

if __name__ == '__main__':
	excel = 'HS300.xlsx'
	opens, closes = get_Data(excel)
	

	ret = getRet(opens,closes,30)
	drawdown = maxDrawdown(ret)
	annuReturn = annualizedrReturn(ret)
	annuSharpeRatio = annualizedSharpeRatio(ret)

	
#    drawdown = np.zeros(30)
#    annuReturn = np.zeros(30)
#    annuSharpeRatio = np.zeros(30)


	#for i in range(30):
	#    ret = getRet(opens,closes,i+1)
	#    ret_all[i,:] = ret

	#drawdown = maxDrawdown(ret)
	#annuReturn = annualizedrReturn(ret)
	#annuSharpeRatio = annualizedSharpeRatio(ret)
	x = np.linspace(1,30,30)
	plt.plot(x,)
	    






	