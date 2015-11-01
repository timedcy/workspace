##本部分实现的功能是计算浮动权重
##填写建仓日和计算当日
rm(list=ls(all=TRUE));
start="2015-04-01"
end="2015-04-21"

setwd("Z:/实盘策略/多因子选股模块/数据集/处理后选股/2015-04-21Alpha策略-王政4")

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
  sign<-which(is.element(names(linshi),c("weight","price1","price2")))
  linshi<-linshi[,-sign]
  after_weight_adjust<-linshi
  #name=paste("after_weight_adjust-alpha4","-",as.character(Sys.Date()),".csv",sep="")
  #write.csv(after_weight_adjust,file=name,row.names=F)
  return(after_weight_adjust)
}

trade1<-read.csv("Alpha4-2015-04-20.csv");
name1<-"Alpha策略-王政1-浮动后.csv";
trade2<-read.csv("Alpha4-2015-04-20-backup.csv");
name2<-"Alpha策略-王政1back-浮动后.csv";
file1<-reweight(trade1,start,end,name1);
colnames(file1)<-c("trade_code","weight")
write.csv(file1,file=name1,row.names=F)
file2<-reweight(trade2,start,end,name2);
colnames(file2)<-c("trade_code","industry","mark","weight")
write.csv(file2,file=name2,row.names=F)


