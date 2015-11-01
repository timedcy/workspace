rm(list=ls(all=TRUE));
setwd("Z:/实盘策略/多因子选股模块/code");
source('functiontogether.R');


names<-"hs30080";
if(names=="hs30080"||names=="zz8005"){
  #设定起止日期间的换仓日（开始时间需是换仓日）
  tradeday_factor<-tradeday("2015-03-26","2015-12-30");
  lasttrade<-tradeday_factor[max(which(Sys.Date()>=tradeday_factor))]; 
}else if(names=="month300"){
  start<-as.Date("2015-03-30")
  end<-as.Date("2015-12-30")
  startday<-as.Date("2015-04-01")
  length<-9
  tradeday_month<-tradeday2(start,end,startday,length)
  lasttrade<-tradeday_month[max(which(Sys.Date()>=tradeday_month))];
}

future<-"IF1506.CFE"

futures_amount<-function(lasttrade,names){

  
  filename<-paste("Z:/实盘策略/多因子选股模块/数据集/策略选股/",lasttrade,"-",names,sep="");
  stock<-read.csv(paste(filename,"/",lasttrade,"-",names,".csv",sep=""),stringsAsFactors=F);
  if(any(0==stock['weight'])){
    stock<-stock[-which(stock["weight"]==0),]
  }
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
  sql<-paste("select trade_code,close from dailyprice where datetime=","'",Sys.Date(),"'"," and trade_code in (",name,")",sep="")
  close<-sqlQuery(ch,sql)
  
  stockdata<-merge(close,stock,by="trade_code")
  stockdata["cap"]<-stockdata["close"]*100/stockdata["weight"]
  
  captialmax<-max(stockdata["cap"])
  
  w.start()
  w_wsd_data<-w.wsd(future,"close","ED0D",Sys.Date(),"Fill=Previous")
  future_close<-w_wsd_data$Data[2]
  future_day<-w_wsd_data$Data[[1]]
  w.stop()
  
  future_amount<-captialmax*0.9/300/future_close[[1]]
  
  stockdata[1,"max"]<-captialmax
  stockdata[1,"future_close"]<-as.character(future_day)
  stockdata[2,"future_close"]<-future_close
  stockdata[1,"future_amount"]<-future_amount
  
  
  return(stockdata)
}

data<-futures_amount(lasttrade,names)
filename<-paste("Z:/实盘策略/多因子选股模块/数据集/策略选股/",lasttrade,"-",names,sep="");
#write.csv(data,file=paste(filename,"/futuresamount.csv",sep=""),row.names=F)