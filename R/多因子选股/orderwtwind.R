rm(list=ls(all=TRUE));
setwd("Z:/实盘策略/多因子选股模块/code");
source('functiontogether.R');

#*******************************************
#function generate list for operation
#*******************************************
generatelist<-function(stockrew){
  stockc<-as.factor(stockrew[,1])
  stockn<-as.factor('证券')
  amount<-as.integer(0)
  weight<-as.numeric(stockrew[,2])
  data<-data.frame(证券代码=stockrew[,1],证券名称=stockn,目标数量=amount,目标权重=weight)
  return(data)
}

state2<-function(names){
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\停牌数据\\",Sys.Date(),".csv",sep="")
  stopstock<-read.csv(filename)
  colnames(stopstock)<-c('tradeday','trade_code');
  
  stock<-read.csv("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\2015-04-27-zz8005\\1_afterdays.csv")
  stock<- setweight(stock,stopstock);
  
  stockback<-read.csv("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\2015-04-27-zz8005\\3_backwtsorigin.csv")
  stockback<- setweight(stockback,stopstock);
  
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),"-",names,sep="");
  if(!file.exists(filename)){
    dir.create(filename)
  }
  setwd(filename)
  list<-generatelist(stock);
  write.csv(list,file=paste(filename,"\\",Sys.Date(),"-",names,".csv",sep=""),row.names=F,quote = F,fileEncoding = "UTF-8")
  write.csv(stock,file="2_wts.csv",row.names=F,quote = F,fileEncoding = "UTF-8")
  write.csv(stockback,file="3_backwts.csv",row.names=F,quote = F,fileEncoding = "UTF-8")
}

names<-"zz8005"
state2(names)
