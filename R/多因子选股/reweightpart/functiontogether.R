#查询策略换仓日
tradeday<-function(start,end){
  library("WindR", lib.loc="C:/Program Files/R/R-3.1.1/library")
  w.start()
  w_tdays_data<-w.tdays(start,end)$Data[,1]
  n<-length(w_tdays_data)
  order<-seq(1,n,20)
  tradeday<-w_tdays_data[order]
  w.stop()
  return(tradeday)
} 


#*****************************
#funtion for get stop stocks
#****************************

getstops<-function(date){
  library("WindR", lib.loc="C:/Program Files/R/R-3.1.1/library")
  datetime<-date
  date<-format(date, "%Y%m%d")
  date<-paste("startdate=",date,";enddate=",date, sep="")
  w.start()
  w_wset_data<-w.wset('TradeSuspend',date)[[2]]
  w_wset_data[,"date"]<-datetime
  w_wset_data[,"trade_code"]<-w_wset_data[,"wind_code"]
  w_wset_data<-w_wset_data[,c("date","trade_code")]
  w.stop()
  return(w_wset_data)
}


#*****************************
#function for reweight
#****************************

setweight <- function(stockpick,stopstock){
  
  stops <- merge(stockpick,stopstock,by='trade_code')
  
  if(nrow(stops)>0){       
    stockpick<-stockpick[-which(is.element(stockpick[,'trade_code'],stops[,'trade_code'])),]
    sum<-sum(stockpick[,"weight"])
    stockpick['weight']<-stockpick['weight']/sum
  }
  
  return(stockpick)
}


#*******************************************
#function for calculate today's stock weight
#*******************************************
reweight<-function(trade,start,end){
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
  return(linshi)
}




