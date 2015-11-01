setwd("Z:/team4share/实盘策略/多因子选股模块/code")

#####################################################################
# This part will help us Organize the data the way we need
# tpn here is for making an excel to put the position into the system of wind
# first update the data

library(RODBC)
ch<-odbcConnect('winddata',uid='root',pwd='123')
load("Z:/team4share/实盘策略/多因子选股模块/RData/2015-08-18--zz8005-all-85.RData")
#stock_pick[["2015-01-22"]]<-add
stock<-stock_pick[85:85]
#stock<-stock_pick[length(stock_pick)]

tpn<-NULL
for( i in names(stock)){
  tp1<-stock[[i]]
  tp1[,'weight']<-tp1[,'weight']*0.8
  date<-trade_day_chr[which(trade_day_chr==i)]
  sql2<-paste("SELECT trade_code,open from dailyprice where datetime= '",date,"'")
  stock.open<-sqlQuery(ch,sql2)
  sql4<-paste("SELECT trade_code,amt from winddata.dailyprice WHERE datetime ='",date,"'")
  stock.amt<-sqlQuery(ch,sql4)
  tp1<-merge(tp1,stock.open,by='trade_code',all.x=T)
  tp1<-merge(tp1,stock.amt,by='trade_code',all.x=T)
  tp1[,'trade_code']<-as.character(tp1[,'trade_code'])
  
  x<-nrow(tp1)
  tp1[x+1,'trade_code']<-"IF.CFE"
  
  #(-0.8)*beta 0.8 means we only use the 80% asset for investing on stocks
  #IF.CFE is the trade_code for futures
  
  tp1[x+1,'weight']<-(-0.8)*0.9
  sqlbase<-paste("select datetime,trade_code, open,oi from hs300if0930 where datetime='",date,"'")
  futures<-sqlQuery(ch,sqlbase)
  hsif<-futures[which(futures[,'oi']==max(futures[,'oi'])),'trade_code']  # the contract traded most in that day
  tp1[x+1,'open']<-futures[hsif,'open']
  tp1[x+1,'amt']<-30
  
  tp1[,'datetime']<-date
  tp1<-tp1[intersect(which(tp1[,'amt']>0),which(tp1[,'weight']!=0)),]# make sure we can buy every stock and move weight=0
  tp1[1:(nrow(tp1)-1),'weight']<-tp1[1:(nrow(tp1)-1),'weight']/sum(tp1[1:(nrow(tp1)-1),'weight'])*0.8
  tpn<-rbind(tpn,tp1)
}

tpn<-tpn[,c(1,2,4,6)]


#tpn<-tpn[,c(1,2)]
#filename<-paste("Z:\\",names(stock),"-zz800",sep="")
#dir.create(filename)
setwd("Z:/team4share")

write.csv(tpn,file="zz800.csv",row.names=F)

## after reorganizing the data, we still need put it into the excel
