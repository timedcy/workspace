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
	#为开盘价和收盘价申请矩阵，从nrows-4开始是因为xls机制
	rows = range(4,nrows)
	cols = range(1,ncols,4)
	opens = np.zeros((nrows-4,N))
	closes = np.zeros((nrows-4,N))

	#取得收盘价和收盘价
	for i in range(nrows-4):
		for j in range(N):
			if table.cell(rows[i],cols[j]).value:
				opens[i,j] = float(table.cell(rows[i],cols[j]).value)
			if table.cell(rows[i],cols[j]+1).value:
				closes[i,j] = float(table.cell(rows[i],cols[j]+1).value)
	return opens, closes
	

def getRet(opens, closes, n):
	global nrows,ncols
	#当天价格表现
	ratio = (closes - opens)/opens
	#按照收盘价和开盘价变化幅度排序	
	#取得排序后，靠前元素在原矩阵中列的位置
	index = np.argsort(ratio, axis = 1)[:,0:n]	
	#为总资产序列申请空间
	total = np.zeros(nrows-4)	
	#总资产初始为1000000
	total[0] = 1000000
	#为收益率序列申请空间
	ret = np.zeros(nrows-5)
	#为持仓量申请空间，行为日期，列为股票
	amount = np.zeros((nrows-5,n))
	#选取前n支股票
	for i in range(nrows-5):
		#将当天资金余额平均分配到每一支股票上
		amount[i] = (total[i]/n)/closes[i,index[i,:]]
		#第二天以开盘价卖出股票，取得资金
		total[i+1] = sum(amount[i,:]*opens[i+1,index[i,:]])
		#计算收益率
		ret[i] = (total[i+1]-total[i])/total[i]
	#返回收益率序列		
	return pd.Series(ret)
	
def maxDrawdown(ret):
	for i in range(len(ret)):
		#如果为NAN赋值为0
		if ret[i] is np.nan:
			ret[i] = 0
	#累计值序列
	cum = ret.cumsum()
	#累计值序列减去样本累计最大值
	drawdown = cum - cum.cummax()
	#返回最大回撤
	return min(drawdown)
#年化收益率
def annualizedrReturn(ret):
	return ret.mean()*250
#年化夏普率
def annualizedSharpeRatio(ret):
	return ret.mean()/ret.std()*np.sqrt(250)


if __name__ == '__main__':
	#取得hs300的价格数据
	excel = 'HS300.xlsx'
	opens, closes = get_Data(excel)
	
	#取得指标 ret每日收益率  drawdown 最大回撤 annuReturn 年化收益 annuSharpeRatio 年化夏普率
	ret = getRet(opens,closes,30)
	drawdown = maxDrawdown(ret)
	annuReturn = annualizedrReturn(ret)
	annuSharpeRatio = annualizedSharpeRatio(ret)
	
	#return_all 总资产变化路径
	return_all = np.zeros(484)
	return_all[0] = 1000000
	for i in range(483):
	   return_all[i] = 100*(1+ret[i])

	#资产变化曲线图
	x = np.linspace(1,484,484)
	plt.plot(x,return_all)
	plt.xlabel('time')
	plt.ylabel('yield')	
	plt.show()


#投资组合数量的敏感性分析  组合数量从1变化到30	
#	    drawdown = np.zeros(30)
#	    annuReturn = np.zeros(30)
#	    annuSharpeRatio = np.zeros(30)
#	for i in range(30):
#	    ret = getRet(opens,closes,i+1)  
#	    ret_all[i,:] = ret
#		drawdown[i] = maxDrawdown(ret)
#		annuReturn[i] = annualizedrReturn(ret)
#		annuSharpeRatio[i] = annualizedSharpeRatio(ret)	
#
#	x = np.linspace(1,30,30)
#	plt.plot(x,drawdown)
#   plt.title('change of maxDrawdown')
#	plt.xlabel('number of stock')
#	plt.ylabel('maxDrawdown')	
#	plt.show()    






	