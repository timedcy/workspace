##本部分实现的功能是计算浮动权重
##填写建仓日和计算当日
rm(list=ls(all=TRUE));
start="2015-03-27"
end="2015-04-20"

setwd("Z:/实盘策略/多因子选股模块/数据集/处理后选股/2015-04-20Alpha策略-王政7")

reweight<-function(trade,start,end,name){
  library(RODBC)
  ch<-odbcConnect("winddata",uid="root",pwd="123");
  sql1<-paste("select trade_code,datetime,open,high,low,close from dailyprice where datetime=","'",start,"'",sep="")
  sql2<-paste("select trade_code,datetime,open,high,low,close from dailyprice where datetime=","'",end,"'",sep="")
  price1<-sqlQuery(ch,sql1)
  price2<-sqlQuery(ch,sql2)
  price1<-price1[,c("trade_code","close")]
  price2<-price2[,c("trade_code","close")]
  price<-merge(price1,price2,by="trade_code")
  colnames(price)<-c("trade_code","price1","price2")
  
  linshi<-merge(trade,price,by="trade_code")
  linshi[,"weight2"]<-linshi[,"weight"]*linshi[,"price2"]/linshi[,"price1"]
  linshi[,"weight2"]<-linshi[,"weight2"]/sum(linshi[,"weight2"])
  linshi<-linshi[,c("trade_code","weight2")]
  colnames(linshi)<-c("trade_code","weight")
  after_weight_adjust<-linshi
  #name=paste("after_weight_adjust-alpha4","-",as.character(Sys.Date()),".csv",sep="")
  write.csv(after_weight_adjust,file=name,row.names=F)
}

trade1<-read.csv("Alpha策略-王政7wts.csv");
name1<-"Alpha策略-王政7-浮动后.csv";
trade2<-read.csv("Alpha策略-王政7backupwts.csv");
name2<-"Alpha策略-王政7back-浮动后.csv";
reweight(trade1,start,end,name1);
reweight(trade2,start,end,name2);