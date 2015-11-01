#每期选股结果
stockpickresult<-function(Os,spool,top){
  library(RODBC)
  ch=odbcConnect("winddata","root","123")
  stock_pick<-list()
  ind_fct<-list()
  
  for (i in Os){
    if(i+L>TT){
      break;pi
    }
    TestPeriod<-seq(i,i+L-1,1)                 
    TradePeriod<-seq(i+L,min(i+L+H-1,TT),1)
    chg<-chg_date[TestPeriod]
    thisdate=max(chg)
    indexdate=sqlQuery(ch,paste("select distinct datetime from",spool))[,1]
    minindexdate=min(indexdate)
    if(thisdate<=minindexdate) {thistradedate=minindexdate} else{thistradedate=max(indexdate[indexdate<thisdate])}
    pick=sqlQuery(ch,paste("select * from",spool,"where datetime='",thistradedate,"'"))[,c(2)]  ##select stock_pick
    pick=as.data.frame(pick)
    colnames(pick)=c("trade_code")
    code_industry<-as.data.frame(industry)
    #pick=as.character(pick)
    pick<-merge(pick,code_industry,by.x="trade_code",by.y="Code")
    pick[,1]=as.character(pick[,1])
    for (j in ind_set){
      stock_pool<-intersect(code_industry[which(code_industry[,3]==j),1],pick[,1]) #股票池
      if(length(stock_pool)>2){
        chg<-chg_date[TestPeriod]
        aaa<-factorpick1(fct_name,chg,stock_pool)
        if(class(aaa)!='numeric'){
          aaba<-cbind(ind_fct[[j]],aaa[,1])
          colnames(aaba)[ncol(aaba)]<-chg_date[i+H]
          ind_fct[[j]]<-aaba      
          fct<-list()      
          for (jj in chg_date[TradePeriod]){
            fct[[jj]]<-aaa             
          }
          bbb<-stockpick_1(fct,stock_pool,top,j) #选股百分比
          for(jjj in names(bbb)){
            aaca<-bbb[[jjj]]
            aaca['industry']<-j
            aaca['weight']<-aaca['weight']*weight[j]
            stock_pick[[jjj]]<-rbind(stock_pick[[jjj]],aaca)
          }
        }           
      }    
    }
    print(i)
    mark<-i
  }
  
  for (jjj in names(stock_pick)){
    aacf<-stock_pick[[jjj]]
    aacf['weight']<-aacf['weight']/sum(aacf['weight'])
    stock_pick[[jjj]]<-aacf
  } #权重
  
  return(list(stock_pick,mark))
}