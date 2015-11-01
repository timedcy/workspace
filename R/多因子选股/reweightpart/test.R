rm(list=ls(all=TRUE));
setwd("Z:/实盘策略/多因子选股模块/code/reweightpart");
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


#state1非换仓日建仓，浮动权重处理后前一天晚上停牌处理后数据
state1<-function(lasttrade,names){
  stopstock<-getstops(lasttrade);
  colnames(stopstock)<-c('tradeday','trade_code');
  
  #清单以及备选清单的换仓日的停牌处理
  filename<-paste("Z:/实盘策略/多因子选股模块/数据集/策略选股/",lasttrade-1,"-",names,sep="");
  stock<-read.csv(paste(filename,"/",lasttrade-1,"-",names,".csv",sep=""),stringsAsFactors=F);
  stock[,"weight"]<-as.numeric(stock[,"weight"])
  stockback<-read.csv(paste(filename,"/",lasttrade-1,"-",names,"-backup.csv",sep=""),stringsAsFactors=F);
  stockback[,"weight"]<-as.numeric(stockback[,"weight"])
  stockrew<- setweight(stock,stopstock);
  backuprew<- setweight(stockback,stopstock);
  
  #新建文件夹
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),"-",names,sep="");
  if(!file.exists(filename)){
    dir.create(filename)
  }
  setwd(filename)
  
  #浮动权重处理(只需要清单的处理)
  stock_afterdays<-reweight(stockrew,lasttrade-1,Sys.Date())
  write.csv(stock_afterdays,"1_afterdays.csv",row.names=F,quote = F)
  
  #清单以及备选清单的今日停牌处理
  stopstock2<-getstops(Sys.Date());
  stockrew2<- setweight(stockrew,stopstock2);
  #backuprew2<- setweight(backuprew,stopstock2);
  write.csv(stockrew2,file="2_wts.csv",row.names=F,quote = F)
  write.csv(backuprew,file="3_backwtsorigin.csv",row.names=F,quote = F)
  #write.csv(backuprew,"4_backwtsorigin.csv",row.names=F,quote = F)
  #交易清单生成
  list<-generatelist(stockrew2);
  write.csv(list,file=paste(Sys.Date(),"-",names,".csv",sep=""),row.names=F,quote = F)
}

#非换仓日建仓，建仓当天停牌处理
state2<-function(names){
  stopstock<-getstops(Sys.Date());
  colnames(stopstock)<-c('tradeday','trade_code');
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date()-1,"-",names,sep="");
  stock<-read.csv(paste(filename,"\\1_afterdays.csv",sep=""))
  stock<- setweight(stock,stopstock);
  stockback<-read.csv(paste(filename,"\\3_backwtsorigin.csv",sep=""))
  stockback<- setweight(stockback,stopstock);
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),names,sep="");
  if(!file.exists(filename)){
    dir.create(filename)
  }
  setwd(filename)
  list<-generatelist(stock);
  write.csv(list,file=paste(filename,"\\",Sys.Date(),"-",names,".csv",sep=""),row.names=F,quote = F)
  write.csv(stock,file="2_wts.csv",row.names=F,quote = F)
  write.csv(stockback,file="3_backwts.csv",row.names=F,quote = F)
}

#建仓日建仓 前一天停牌处理
state3<-function(names){
  #停牌数据提取
  stopstock<-getstops(Sys.Date());
  colnames(stopstock)<-c('tradeday','trade_code');
  
  #清单以及备选清单的换仓日的停牌处理
  filename<-paste("Z:/实盘策略/多因子选股模块/数据集/策略选股/",Sys.Date(),"-",names,sep="");
  stock<-read.csv(paste(filename,"/",Sys.Date(),"-",names,".csv",sep=""),stringsAsFactors=F);
  stock[,"weight"]<-as.numeric(stock[,"weight"])
  stockback<-read.csv(paste(filename,"/",Sys.Date(),"-",names,"-backup.csv",sep=""),stringsAsFactors=F);
  stockback[,"weight"]<-as.numeric(stockback[,"weight"])
  stockrew<- setweight(stock,stopstock);
  backuprew<- setweight(stockback,stopstock);
  list<-generatelist(stockrew);
  #新建文件夹
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),"-",names,sep="");
  if(!file.exists(filename)){
    dir.create(filename)
  }
  setwd(filename)
  
  write.csv(list,file=paste(names,"-",Sys.Date(),".csv",sep=""),row.names=F,quote = F)
  write.csv(stockrew,file=paste(filename,"\\","1_wts.csv",sep=""),row.names=F,quote = F)
  write.csv(stockrew,file=paste(filename,"\\","2_backwts.csv",sep=""),row.names=F,quote = F)
}

#换仓日 交易当天的停牌处理
state4<-function(names){
  stopstock<-getstops(Sys.Date());
  colnames(stopstock)<-c('tradeday','trade_code');
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date()-1,"-",names,sep="");
  stock<-read.csv(paste(filename,"\\","1_wts.csv",sep=""))
  stockback<-read.csv(paste(filename,"\\","2_backwts.csv",sep=""))
  stock<- setweight(stock,stopstock);
  stockback<- setweight(stockback,stopstock);
  list<-generatelist(stock);
  
  filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),names,sep="");
  if(!file.exists(filename)){
    dir.create(filename)
  }
  setwd(filename)
  
  write.csv(list,file=paste(names,"-",Sys.Date(),".csv",sep=""),row.names=F,quote = F)
  write.csv(stock,file=paste(filename,"\\","1_wts.csv",sep=""),row.names=F,quote = F)
  write.csv(stockback,file=paste(filename,"\\","2_backwts.csv",sep=""),row.names=F,quote = F)
}


#设定起止日期间的换仓日（开始时间需是换仓日）
tradeday<-tradeday("2015-03-27","2015-12-30");
lasttrade<-tradeday[max(which(Sys.Date()>=tradeday))];
names<-"hs30080";

# 
state1(lasttrade,names)
#state2(names)
# state3(names)
#state4(names)