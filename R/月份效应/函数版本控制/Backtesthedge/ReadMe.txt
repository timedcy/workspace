函数说明：
Backtesthedge1=function(cap,Invr,beta,stock,fee.ratio1,fee.ratio2,tax.ratio,impact.ratio1,impact.ratio2,plusday)
Backtesthedge2=function(cap,Invr,beta,stock,fee.ratio1,fee.ratio2,tax.ratio,impact.ratio1,impact.ratio2,lastday)
Backtesthedge2对Backtesthedge1的改进有三点：
第一：Backtesthedge1一直用股指对冲，Backtesthedge2在2010-04-16前用股指对冲，在2010-04-16后改用股指期货对冲。
第二：将plusday=30改为lastday="2015-01-30"
第三：Backtesthedge2返回的caplist为两个list,第一个为Backtesthedge1中的list，第二个为交易总成本。

两个回测函数均为在换仓日，按照收盘价计算要买的股票数，而在换仓日第二天按照开盘价以前一天计算的股票数买入股票。

plot_cal=function(capital,date,t='the trend of revenue',rho=0.03,cex=0.7,col='black')
绘图模板，只是将原模板中的中文改为英文。