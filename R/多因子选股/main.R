setwd("Z:/team4share/实盘策略/多因子选股模块/code5")
source("f-getdata.R")
source("newdata.R")
source('f-factorpick.R')
source('f-stockpick.R')
source('stockpickresult.R')

#设定区间，起点，股票池
TT<-length(chg_date)
L<-24  #筛选因子的所用数据长度
H<-3   #因子有效期
O<-85   #起点
Os<-seq(O,TT-L+1,H)
spool<-"dailyprice"
#if(setequal(spool,zz300)){
  #poolname<-"-hs300"
  #top<-0.8
  #topb<-1
  #percent<-top*100
#}
#if(setequal(spool,zz800)){
  #poolname<-"-zz800"
  #top<-0.05
  #topb<-1
  #percent<-top*100
#}
if(spool=="dailyprice"){
  poolname<-"-dailyprice"
  top<-0.04
  topb<-1
  percent<-top*100
}

#存储选股结果，期数
list<-stockpickresult(Os,spool,top)
stock_pick<-list[[1]]
mark<-list[[2]]

#O=1 存储历史至今每期选股
#else 存储当期选股以及备选股 
if(O==1){
  save(stock_pick,file=paste("Z:\\team4share\\zxt\\code5\\",poolname,percent,"-all-",mark,".RData",sep="")) 
}else{
  stock_pick<-stock_pick[length(stock_pick)]
  #save(stock_pick,file=paste("Z:\\实盘策略\\多因子选股模块\\RData\\",names(stock_pick),"-",poolname,percent,"-",mark,".RData",sep="")) 
  save(stock_pick,file=paste("Z:\\team4share\\实盘策略\\多因子选股模块\\RData\\",Sys.Date(),"-",poolname,percent,"-",mark,".RData",sep="")) 
  stock_pick_backup<-stockpickresult(Os,spool,topb)[[1]]
  stock_pick_backup<-stock_pick_backup[[length(stock_pick_backup)]]
  stock_pick_backup[which(is.element(stock_pick_backup[,1],stock_pick[[1]][,1])),"5%"]<-1
  stock_pick_backup[which(!is.element(stock_pick_backup[,1],stock_pick[[1]][,1])),"5%"]<-0 
  
  filename<-paste("Z:\\team4share\\实盘策略\\多因子选股模块\\数据集\\策略选股\\",names(stock_pick),poolname,percent,sep="")
 # filename<-paste("Z:\\zxt\\code4\\",Sys.Date(),poolname,percent,sep="")
  if(!file.exists(filename)){
    dir.create(filename)
  }
  
  stock_pick<-stock_pick[[1]]
  if(is.element(0,stock_pick["weight"])){
   stock_pick<-stock_pick[-which(stock_pick["weight"]==0),]
  }

  #write.csv(stock_pick[,1:2],file=paste(filename,"\\",Sys.Date(),poolname,percent,".csv",sep=""),row.names=F)
  write.csv(stock_pick[,1:2],file=paste(filename,"\\",Sys.Date(),poolname,percent,".csv",sep=""),row.names=F)
  write.csv(stock_pick_backup,file=paste(filename,"\\",Sys.Date(),poolname,percent,"-backup.csv",sep=""),row.names=F)

}




# 1.起始资本 2.起始投资比例（默认0.8可能变化） 3.对冲比例 4.每日选股 5.股票手续费 6.期货手续费 7.印花税 8.股票冲击成本 9.期货的冲击成本 10.额外天数
#caplist1<-Backtesthedge(30000000,0.80,0.9,stock_pick,0.00035,0.00003,0.001,0.003,0.00017,30)



