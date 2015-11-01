setwd("E:/ZDQH/实盘策略/多因子选股模块/code")

source('f-stop.R');

#*******************************
#function for generate list
#********************************

generate<-function(stockpick,stockpickbackup,names){
  stockrew<- setweight(stockpick,stopstock);
  backuprew<- setweight(stockpickbackup,stopstock);
  
  filename<-paste("E:\\ZDQH\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),names,sep="");
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
stopstock<-getstops(date);
write.csv(stopstock,file=paste("Z:\\实盘策略\\多因子选股模块\\停牌数据\\2015-04-20.csv",date,".csv",sep=""),row.names=F);

names<-"Alpha策略-王政4";
zz800pick<-read.csv("Z:/实盘策略/多因子选股模块/数据集/策略选股/2015-04-20-zz8005/2015-04-20-zz8005.csv");
zz800pickbackup<-read.csv("Z:/实盘策略/多因子选股模块/数据集/策略选股/2015-04-20-zz8005/2015-04-20-zz8005-backup.csv");
generate(zz800pick,zz800pickbackup,names);

# names<-"Alpha策略-王政7";
# hs300pick<-read.csv("Z:/实盘策略/多因子选股模块/数据集/策略选股/2015-03-26-hs30080/2015-03-26-hs30080.csv")
# hs300pickbackup<-read.csv("Z:/实盘策略/多因子选股模块/数据集/策略选股/2015-03-26-hs30080/2015-03-26-hs30080-backup.csv")
# generate(hs300pick,hs300pickbackup,names);

