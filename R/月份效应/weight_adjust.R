##本部分实现的功能是计算浮动权重
##填写建仓日和计算当日
# start="2015-03-02"
# end="2015-03-26"

##此部分的代码不用动
rm(list=ls(all=TRUE));
start="2015-02-26"
end="2015-04-09"

#setwd("G:\\R program&data\\final\\tradeT0");
setwd("G:\\R program&data\\final\\tradeT1");
library(RODBC)
ch=odbcConnect("winddata",uid="root",pwd="123");
sql1=paste("select trade_code,datetime,open,high,low,close from dailyprice where datetime=","'",start,"'",sep="")
sql2=paste("select trade_code,datetime,open,high,low,close from dailyprice where datetime=","'",end,"'",sep="")
price1=sqlQuery(ch,sql1)
price2=sqlQuery(ch,sql2)
price1=price1[,c("trade_code","close")]
price2=price2[,c("trade_code","close")]
price=merge(price1,price2,by="trade_code")
colnames(price)=c("trade_code","price1","price2")

##读取要调整权重的组合
name=paste("adjust-trade-alpha4","-",as.character(Sys.Date()),".csv",sep="")
trade1=read.csv(file=name)
linshi=merge(trade1,price,by="trade_code")
linshi[,"weight2"]=linshi[,"weight"]*linshi[,"price2"]/linshi[,"price1"]
linshi[,"weight2"]=linshi[,"weight2"]/sum(linshi[,"weight2"])
linshi=linshi[,c("trade_code","weight2")]
colnames(linshi)=c("trade_code","weight")
after_weight_adjust=linshi
name=paste("after_weight_adjust-alpha4","-",as.character(Sys.Date()),".csv",sep="")
write.csv(after_weight_adjust,file=name,row.names=F)

