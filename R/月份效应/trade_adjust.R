extra_stop_stock=function() {
setwd("Z:\\多因子选股模块\\RData")
load("2015-03-31-zz300-79.RData")
n=length(stock_pick)
this_trade=stock_pick[[n]]


load("2015-03-30-zz800-79.RData")
n=length(stock_pick)
this_trade=stock_pick[[n]]



##该部分内容实现的功能是剔除选定的投资组合中的停牌股票（运行该程序当时停牌的股票）
##删除之前的所有变量
rm(list=ls(all=TRUE));
#setwd("G:\\R program&data\\final\\tradeT0"); ##T0运行时，选择这一行
setwd("G:\\R program&data\\final\\tradeT1");  ##T1运行时，选择这一行

##加载wind包，并且提取当时的停牌股票
library("WindR", lib.loc="C:/Program Files/R/R-3.1.1/library")
datetime<-Sys.Date()
date<-format(Sys.Date(), "%Y%m%d")
date<-paste("startdate=",date,";enddate=",date, sep="")
w.start()
w_wset_data<-w.wset('TradeSuspend',date)[[2]]
w_wset_data[,"日期"]<-datetime
w_wset_data[,"Wind代码"]<-w_wset_data[,"wind_code"]
w_wset_data<-w_wset_data[,c("日期","Wind代码")]
colnames(w_wset_data)=c("datetime","trade_code")
write.csv(w_wset_data,file=paste("stop","-",datetime,".csv",sep=""),row.names=F)#将提取的停牌股票存放
w.stop()


setwd("G:\\R program&data\\final\\tradeT0"); #从tradeT0中提取T0时选定的股票组合
name=paste("trade","-",as.character(Sys.Date()-2),".csv",sep="")
this_trade=read.csv(file=name)
setwd("G:\\R program&data\\final\\tradeT1")
this_stop=w_wset_data
this_stop_code=merge(this_trade,this_stop,by="trade_code")
this_stop_code=this_stop_code[,"trade_code"]
this_trade_code=this_trade[,"trade_code"]
final_trade_code=setdiff(this_trade_code,this_stop_code)
final_trade_code=data.frame(trade_code=final_trade_code)
final_trade=merge(final_trade_code,this_trade,by="trade_code")
final_trade=final_trade[,c("trade_code","weight")]
#权重标准化，将停牌股票的权重分配到其他股票上面，分配的方式有两种：
##第一种：将停牌股票的权重，按照剩余股票的权重进行分配
final_trade[,"weight"]=final_trade[,"weight"]/sum(final_trade[,"weight"]) 
##第二种：将停牌股票的权重，平均分配到剩余股票上
#final_trade[,"weight"]=final_trade[,"weight"]+(1-sum(final_trade[1:,"weight"]))/dim(final_trade)[1] 
name=paste("adjust-trade","-",as.character(Sys.Date()),".csv",sep="")
write.csv(final_trade,file=name,row.names=F)
}

