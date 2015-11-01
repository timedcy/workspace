setwd("F:/team4share/zxt/code5")
source("f-getdata.R")
source("newdata.R")
source('f-factorpick.R')
source('f-stockpick.R')
source('stockpickresult.R')
source("Backtesthedge5.R")
source("Part4 plot.R")
source("sen_a.R")


#设定区间，起点，股票池
TT<-length(chg_date)
L<-24  #筛选因子的所用数据长度
H<-3   #因子有效期
O<-1   #起点
Os<-seq(O,TT-L+1,H)
spool<-"dailyprice"

topn=seq(0.01,0.15,by=0.01)
stock_pickn=list()
lll=0

for(top in topn) {
  lll=lll+1
  print(top)
  list<-stockpickresult(Os,spool,top)
  stock_pick<-list[[1]]
  name=paste("stock_pick","_",top,".RData",sep="")
  save(stock_pick,file=name)
  stock_pickn[[lll]]=stock_pick  
}


load("stock_pick_0.01.RData")
s1<-stock_pick
load("stock_pick_0.02.RData")
s2<-stock_pick
stock_pickn[[1]]<-s1
stock_pickn[[2]]<-s2



theresult1=sensibility_analysis(stock_pickn,topn,0.9,name="allA_zz500weight_zz500ic",impact=0.004,lastday="2015-06-03",zz500ic="zz500ic0930")
write.csv(theresult1,file="allA_zz500weight_zz500ic_top_sensibility.csv",row.names=F)
