#setwd("Z:\\实盘策略\\回测");
load("Z:/team4share/实盘策略/多因子选股模块/RData/2015-07-21--hs30080-all-84.RData")
hs300stock<-stock_pick
load("Z:/team4share/实盘策略/多因子选股模块/RData/2015-08-18--hs30080-85.RData")
hs300stock_1<-stock_pick
c=c(hs300stock,hs300stock_1)
stock_pick<-c
#save(stock_pick,file=paste("Z:/team4share/实盘策略/多因子选股模块/RData/","allA4","-all-",85,".RData",sep="")) 


save(stock_pick,file="Z:/team4share/实盘策略/多因子选股模块/RData/2015-08-18--hs30080-all-85.RData") 












