setwd("Z:/实盘策略/多因子选股模块/code")

source('f-stop.R')
#*****************************
#stop stock reweight
#****************************
#读取停牌数据
date<-Sys.Date()
#date<-as.Date("2015-04-13")
stopstock<-getstops(date)
write.csv(stopstock,file=paste("Z:\\实盘策略\\多因子选股模块\\停牌数据\\",date,".csv",sep=""),row.names=F)

# zz800pick<-read.csv("Z:/实盘策略/多因子选股模块/数据集/策略选股/2015-03-26-zz8005/2015-03-26-zz8005.csv")
# zz800pickbackup<-read.csv("Z:/实盘策略/多因子选股模块/数据集/策略选股/2015-03-26-zz8005/2015-03-26-zz8005-backup.csv")
zz800picK<-read.csv("Z:\\实盘策略\\多因子选股模块\\计算\\327-416\\alpha1-401-416-reweighted.csv")
monthpick<-read.csv("Z:\\实盘策略\\月份效应\\tradeT0\\trade-2015-03-31.csv")
#hs300f <- setweight(hs300pick,stopstock)
zz800f <- setweight(zz800picK,stopstock)
#zz800fb<- setweight(zz800pickbackup,stopstock)

monthf<-setweight(monthpick,stopstock)

filename<-paste("Z:\\实盘策略\\多因子选股模块\\数据集\\处理后选股\\",Sys.Date(),sep="")
if(!file.exists(filename)){
  dir.create(filename)
}
save(hs300f,file="hs300wtstop.RData")
stocknames<-paste("Z:/实盘策略/多因子选股模块/数据集/处理后选股/",Sys.Date(),"/zz8005wts", sep="")
save(zz800f,file=paste(stocknames,".RData",sep=""))
save(monthf,file=paste(stocknames,".RData",sep=""))
write.csv(hs300f,file=paste(stocknames,"Alpha策略-王政7.csv",sep=""),row.names=FALSE)
write.csv(zz800f,file=paste(stocknames,"Alpha策略-王政1.csv",sep=""),row.names=FALSE)
write.csv(monthf,file=paste(stocknames,"Alpha策略-王政4.csv",sep=""),row.names=FALSE)


#********************************
#清单格式生成
#********************************
#stock<-read.csv("F:\\team4share\\实盘策略\\多因子选股模块\\计算\\327-410\\Alpha策略-王政1.csv",header = T)

  stock<-zz800f
  stock<-monthf
  stockc<-as.factor(stock[,1])
  stockn<-as.factor('证券')
  amount<-as.integer(0)
  weight<-as.numeric(stock[,2])
  data<-data.frame(证券代码=stock[,1],证券名称=stockn,目标数量=amount,目标权重=weight)
  write.csv(data,"Z:\\实盘策略\\实盘记录\\数据集\\zz8005-alpha2\\test.csv",row.names=F,quote = F)

  write.csv(data,file=paste(stocknames,"Alpha策略-王政4清单.csv",sep=""),row.names=FALSE)
