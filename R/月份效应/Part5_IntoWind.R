IntoWind=function(stock_pick,beta) {
  stock<-stock_pick[65:128]
  
  library(RODBC)
  ch<-odbcConnect('winddata',uid='root',pwd='123');
  trade_day=sqlQuery(ch,'select distinct datetime from dailyprice where datetime > "2010-04-30"')[,1]
  indexdate<-as.Date(trade_day)
  
  trade_day=as.character(trade_day)
  #stock=stock_pick
  
  #indexdate<-sqlQuery(ch,paste("select distinct datetime from",pick))[,1]
  #indexdate<-as.Date(indexdate)
  
  start_time="2010-05-01"
  date<-seq(as.Date(start_time), length=64, by="month")
  date<-as.Date(date)
  ii=1
  
  tpn<-NULL
  for( i in names(stock)){
    tp1<-stock[[i]]
    tp1[,'weight']<-tp1[,'weight']*0.8
    
      thisdate<-date[ii]
      minindexdate<-min(indexdate[ii])
      ##select stock pick in date[i]
      if (thisdate<=minindexdate) { 
        thisindexdate<-minindexdate } else { 
          thisindexdate<-max(indexdate[indexdate<thisdate+1])}
    ii=ii+1
    
    sql2<-paste("SELECT trade_code,open from dailyprice where datetime='",thisindexdate,"'")
    stock.close<-sqlQuery(ch,sql2)
    sql4<-paste("SELECT trade_code,amt from winddata.dailyprice WHERE datetime ='",thisindexdate,"'")
    stock.amt<-sqlQuery(ch,sql4)
    tp1<-merge(tp1,stock.close,by='trade_code',all.x=T)
    tp1<-merge(tp1,stock.amt,by='trade_code',all.x=T)
    tp1[,'trade_code']<-as.character(tp1[,'trade_code'])
    tp1<-tp1[which(tp1[,'amt']>0),]  # make sure we can buy every stock
    tp1[,'weight']=tp1[,'weight']/sum(tp1[,'weight'])
    tp1[,'weight']<-tp1[,'weight']*0.8
    
    x<-nrow(tp1)
    rownames(tp1)=1:x
    tp1[x+1,'trade_code']<-"IF.CFE"
    
    #(-0.8)*beta 0.8 means we only use the 80% asset for investing on stocks
    #IF.CFE is the trade_code for futures
    
    tp1[x+1,'weight']<-(-0.8)*beta
    sqlbase<-paste("select datetime,trade_code,open,oi from hs300if0930 where datetime='",thisindexdate,"'")
    futures<-sqlQuery(ch,sqlbase)
    hsif<-futures[which(futures[,'oi']==max(futures[,'oi'])),'trade_code']  # the contract traded most in that day
    tp1[x+1,'open']<-futures[hsif,'open']
    tp1[x+1,'amt']<-30
    
    tp1[,'datetime']<-thisindexdate
    tpn<-rbind(tpn,tp1)
  }
 # write.csv(tpn,file=name,row.names=F)
  return(tpn);
  
}
