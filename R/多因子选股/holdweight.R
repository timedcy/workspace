rm(list=ls(all=TRUE));
setwd("Z:/实盘策略/多因子选股模块/code");

stock<-read.csv("Z:\\实盘策略\\实盘记录\\zz8005-alpha1\\2015-4-29\\hold\\2015-04-30-hold.CSV",stringsAsFactors=F);
stockname<-stock[,"trade_code"]
name<-NULL
for(i in stockname){
  if(setequal(i,stockname[1])){
    name<-paste("'",i,"'",sep="")
  }else{
    name<-paste(name,",'",i,"'",sep="")
  }
}


library(RODBC)
ch<-odbcConnect('winddata',uid='root',pwd='123')
sql<-paste("select trade_code,close,amt from dailyprice where datetime=","'",Sys.Date(),"'"," and trade_code in (",name,")",sep="")
close<-sqlQuery(ch,sql)
stockdata<-merge(close,stock,by="trade_code")
if(any(0==stockdata['amt'])){
  stockdata<-stockdata[-which(stockdata["amt"]==0),]
}
cap<-stockdata["close"]*stockdata["amount"]

sum<-sum(cap)
stockdata["weight"]<-cap/sum

filename<-paste("Z:/实盘策略/多因子选股模块/数据集/策略选股/",Sys.Date(),"-Alpha1",sep="");
if(!file.exists(filename)){
  dir.create(filename)
}
setwd(filename)

write.csv(stockdata[,c("trade_code","weight")],file=paste(Sys.Date(),"-Alpha1.csv",sep=""),row.names=F)
