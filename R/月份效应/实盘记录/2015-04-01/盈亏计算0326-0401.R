rm(list=ls())
setwd("G:\\R program&data\\final\\实盘记录\\2015-04-01")
###################################################################
#a2<-read.csv("王政4-20150326-已平.csv",header=T)[,c("STOCK_CODE","STK_AMOUNT","STOP_PRICE")]
a2<-read.csv("王政4-20150326-已平.csv",header=T)[,c("STOCK_CODE","STK_AMOUNT","DETAIL_VALUE","STOP_PRICE")]
n=dim(a2)[1]
myreturn=(a2[,4]-a2[3])*a2[,2]
total=sum(a2[1:(n-1),2]*a2[1:(n-1),3])
total
feiyong=total*0.001
feiyong
thereturn=sum(myreturn[1:(n-1),1])-myreturn[n,1]*300
thereturn
thereturn-feiyong
(thereturn-feiyong)/total
####如果不算手续费的话，这一单赚了22598.45元；
####扣除费用的话赚了21284.54





