##本部分实现的功能是计算浮动权重
##填写建仓日和计算当日

rm(list=ls(all=TRUE));

tran=function(x) {
  n=length(x)
  xx=NULL
  for(i in 1:n) {
    a=x[i]
    if(a<600000) {
      m=nchar(a)
      m=6-m;
      for(j in 1:m) {
        a=paste(0,a,sep="")
      }
      a=paste(a,".SZ",sep="")
    }
    if(a==600000) { a=paste("600000",".SH",sep="") }
    if(a>600000) {a=paste(a,".SH",sep="") }
    a=as.character(a)
    xx=c(xx,a)
  }
  return(xx)
}

##此部分的代码不用动

start="2015-03-31"
end="2015-04-02"

#setwd("G:\\R program&data\\final\\tradeT0");
setwd("G:\\R program&data\\final\\tradeT1");
library(RODBC)
ch=odbcConnect("winddata",uid="root",pwd="123");
ch1=odbcConnect("jydb",uid="jydb",pwd="jydb");
sql1=paste("select trade_code,datetime,open,high,low,close from dailyprice where datetime=","'",start,"'",sep="")
sql2=paste("select trade_code,datetime,open,high,low,close from dailyprice where datetime=","'",end,"'",sep="")
sql11=paste("select b.SecuCode ,b.ChiName,a.TradingDay,a.OpenPrice,a.HighPrice,a.LowPrice,a.ClosePrice ,a.TurnoverVolume,a.TurnoverValue,a.TurnoverDeals from qt_dailyquote a, secumain b where a.innercode = b.innercode and (b. SecuCategory=1 and b.ListedState=1  and b.SecuMarket in (83,90) ) and a.TradingDay=","'",start,"'",sep="")
sql12=paste("select b.SecuCode ,b.ChiName,a.TradingDay,a.OpenPrice,a.HighPrice,a.LowPrice,a.ClosePrice ,a.TurnoverVolume,a.TurnoverValue,a.TurnoverDeals from qt_dailyquote a, secumain b where a.innercode = b.innercode and (b. SecuCategory=1 and b.ListedState=1  and b.SecuMarket in (83,90) ) and a.TradingDay=","'",end,"'",sep="")

price1=sqlQuery(ch1,sql11)
price2=sqlQuery(ch1,sql12)
price1=price1[,c(1,7)]
price2=price2[,c(1,7)]
price1[,1]=tran(price1[,1])
price2[,1]=tran(price2[,1])

colnames(price1)=c("trade_code","price1")
colnames(price2)=c("trade_code","price2")
price=merge(price1,price2,by="trade_code")
colnames(price)=c("trade_code","price1","price2")

##读取要调整权重的组合
name=paste("adjust-trade","-",as.character(Sys.Date()),".csv",sep="")
trade1=read.csv(file=name)
linshi=merge(trade1,price,by="trade_code")
linshi[,"weight2"]=linshi[,"weight"]*linshi[,"price2"]/linshi[,"price1"]
linshi[,"weight2"]=linshi[,"weight2"]/sum(linshi[,"weight2"])
linshi=linshi[,c("trade_code","weight2")]
colnames(linshi)=c("trade_code","weight")
after_weight_adjust=linshi
name=paste("after_weight_adjust","-",as.character(Sys.Date()),".csv",sep="")
write.csv(after_weight_adjust,file=name,row.names=F)
