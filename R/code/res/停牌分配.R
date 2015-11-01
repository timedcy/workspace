setwd("Z:/实盘策略/多因子选股模块")
#读取停牌数据
stopstock<-paste("Z:/实盘策略/多因子选股模块/停牌数据/",Sys.Date(),".csv", sep="")
stopstock<-read.csv(stopstock,header = T)

#lode数据集合
# load(paste("Z:/ZDQH/实盘策略/多因子选股模块/RData/",Sys.Date(),"-hs300-79.RData",sep=""))
# stockpick<-stock_pick[[length(stock_pick)]]
# stockpick<-stockpick[,1:2]
hs300pick<-read.csv("F:\\team4share\\实盘策略\\多因子选股模块\\计算\\327-410\\hs300wts-327-reweighted.csv",header = T)
#hs300pick=hs300pick[,1:2]
zz800pick<-read.csv("F:\\team4share\\实盘策略\\多因子选股模块\\计算\\327-410\\zz8005wts-327-reweighted.csv",header = T)
monthpick<-read.csv("F:\\team4share\\实盘策略\\多因子选股模块\\计算\\327-410\\alpha4-401-reweighted.csv",header = T)



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

hs300f <- setweight(hs300pick,stopstock)
zz800f <- setweight(zz800pick,stopstock)
monthf<-setweight(monthpick,stopstock)

save(hs300f,file="hs300wtstop.RData")
stocknames<-paste("Z:/多因子选股模块/数据集/",Sys.Date()," zz800wts5", sep="")
save(zz800f,file=paste(stocknames,".RData",sep=""))

write.csv(hs300f,file="Alpha策略-王政7.csv",row.names=FALSE)
write.csv(zz800f,file="Alpha策略-王政1.csv",row.names=F)
write.csv(monthf,file="Alpha策略-王政4.csv",row.names=F)
