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
  w_wset_data[,"日期"]<-datetime
  w_wset_data[,"Wind代码"]<-w_wset_data[,"wind_code"]
  w_wset_data<-w_wset_data[,c("日期","Wind代码")]
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


