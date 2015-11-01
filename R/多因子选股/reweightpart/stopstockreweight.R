rm(list=ls(all=TRUE));
setwd("Z:/实盘策略/多因子选股模块/code/reweightpart")

source('f-stop.R');


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
  write.csv(stockrew,file=paste(names,"wts.csv",sep=""),row.names=FALSE)
  write.csv(backuprew,file=paste(names,"backupwts.csv",sep=""),row.names=FALSE)
  
  stockc<-as.factor(stockrew[,1])
  stockn<-as.factor('证券')
  amount<-as.integer(0)
  weight<-as.numeric(stockrew[,2])
  data<-data.frame(证券代码=stockrew[,1],证券名称=stockn,目标数量=amount,目标权重=weight)
  
  write.csv(data,file=paste(file=paste(names,".csv",sep="")),row.names=F,quote = F)
}



#读取停牌数据
 date<-Sys.Date();
# #date<-as.Date("2015-03-27")
 stopstock<-getstops(date);
#stopstock<-read.csv(file="Z:\\实盘策略\\多因子选股模块\\停牌数据\\2015-04-21.csv")
write.csv(stopstock,file=paste("z:\\实盘策略\\多因子选股模块\\停牌数据\\",date,".csv",sep=""),row.names=F);
colnames(stopstock)<-c('tradeday','trade_code')
setwd("Z:/实盘策略/多因子选股模块/数据集/处理后选股/2015-04-21Alpha策略-王政1")
names<-"Alpha策略-王政1-422";

#setwd("D:\\zdqh\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\2015-04-21Alpha策略-王政4")
#names<-"Alpha策略-王政4-421";
zz800pick<-read.csv("Alpha策略-王政1-浮动后.csv",stringsAsFactors=F);
zz800pick[,"weight"]<-as.numeric(zz800pick[,"weight"])
zz800pickbackup<-read.csv("Alpha策略-王政1back-浮动后.csv",stringsAsFactors=F);
zz800pickbackup[,"weight"]<-as.numeric(zz800pickbackup[,"weight"])
generate(zz800pick,zz800pickbackup,names);



