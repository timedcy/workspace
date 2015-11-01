setwd("Z:\\实盘策略\\多因子选股模块\\停牌数据")
library("WindR", lib.loc="C:/Program Files/R/R-3.1.1/library")
datetime<-Sys.Date()
date<-format(Sys.Date(), "%Y%m%d")
date<-paste("startdate=",date,";enddate=",date, sep="")
w.start()
w_wset_data<-w.wset('TradeSuspend',date)[[2]]
w_wset_data[,"日期"]<-datetime
w_wset_data[,"Wind代码"]<-w_wset_data[,"wind_code"]
w_wset_data<-w_wset_data[,c("日期","Wind代码")]
write.csv(w_wset_data,file=paste(datetime,".csv",sep=""),row.names=F)
w.stop()
