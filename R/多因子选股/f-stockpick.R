
stockpick_1<-function(fct,stock_pool,top=0.1,j){
  stock_equ<-list()
  
  #k日期
  for(k in names(fct)){  
    thisdate=k
    indexdate=sqlQuery(ch,paste("select distinct datetime from",spool))[,1]
    minindexdate=min(indexdate)
    if(thisdate<=minindexdate) {thistradedate=minindexdate} else{thistradedate=max(indexdate[indexdate<thisdate])}
    pick=sqlQuery(ch,paste("select * from",spool,"where datetime='",thistradedate,"'"))[,c(2)]  ##select stock_pick
    pick=as.data.frame(pick)
    colnames(pick)=c("trade_code")
    code_industry<-as.data.frame(industry)
    pick[,1]=as.character(pick[,1])
    #pick=as.character(pick)
    pick<-merge(pick,code_industry,by.x="trade_code",by.y="Code")
    stock_pool<-intersect(code_industry[which(code_industry[,3]==j),1],pick[,1])
   #ppp股票代码以及其分数的表格
   ppp<-data.frame(stock_pool,row.names = stock_pool)
   colnames(ppp)<-c('trade_code')
   ppp['score']<-0
   
   #kk因子名
   for(kk in fct[[k]][,1]){ 
     a<-fctdata[[k]][,c('trade_code',kk)]       #code + factor value
     rownames(a)<-a[,1]
     a<-a[stock_pool,]                          #a:stock in stockpool with factor value
     a<-a[complete.cases(a[,2]),]               #rm factor value=na
     
     #根据：因子值*方向/(a的个数/10)*因子分数 排序，取整
     a['score']<-floor(rank(a[,2]*fct[[k]][kk,'decreasing'])/(nrow(a)/10))*fct[[k]][kk,'score']
     fcode<-intersect(ppp[,1],a[,1])            #股票池中有因子值的股票代码
     ppp[fcode,'score']<-ppp[fcode,'score']+a[fcode,'score']  #累加因子分数到股票分数
   }
   pppp<-ppp[order(ppp[,2],decreasing = T)[1:ceiling(nrow(ppp)*top)],1,drop=F] #取top股票
   pppp['weight']<-1/nrow(pppp) #股票均分
   stock_equ[[k]]<-pppp
  }
  return(stock_equ)
}