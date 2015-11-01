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
  
  stopstock<- stopstock[,c("日期","Wind代码")]
  colnames(stopstock)<-c('tradeday','trade_code')
  colnames(stockpick)<-c('trade_code','weight')  
  stops <- merge(stockpick,stopstock,by='trade_code')
  
  if(nrow(stops)>0){   
    for(i in stops[,'trade_code']){
      stockpick<-stockpick[-which(stockpick[,'trade_code']==i),]
    }
    sum<-sum(stockpick[,"weight"])
    stockpick['weight']<-stockpick['weight']/sum
  }
  
  return(stockpick)
}


#*******************************
#function for generate list
#********************************

generate<-function(stockpick,stockpickbackup,names){
  stockrew<- setweight(stockpick,stopstock);
  backuprew<- setweight(stockpickbackup,stopstock);
  
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),names,sep="");
  if(!file.exists(filename)){
    dir.create(filename)
  }
  setwd(filename);
  save(stockrew,file=paste(names,"wts.RData",sep=""));
  save(backuprew,file=paste(names,"backupwts.RData",sep=""));
  write.csv(stockrew,file=paste(names,"wtscsv",sep=""),row.names=FALSE)
  write.csv(backuprew,file=paste(names,"backupwts.csv",sep=""),row.names=FALSE)
  
  stockc<-as.factor(stockrew[,1])
  stockn<-as.factor('证券')
  amount<-as.integer(0)
  weight<-as.numeric(stock[,2])
  data<-data.frame(证券代码=stock[,1],证券名称=stockn,目标数量=amount,目标权重=weight)
  
  write.csv(data,file=paste(file=paste(names,".csv",sep="")),row.names=F,quote = F)
}



