dyna_hedge=function(caplist,stock_pick,beta,impact.ratio,date,hs300if) {
  threshold=0.05
  caplist2=Backtesthedge4(30000000,0.85,beta,stock_pick,0.00035,0.00003,0.001,impact.ratio,0.00017,date,0.05,"hs300if0930")[[1]]
  
  nv_1=caplist2[,10]
  nv=caplist[,10]
  library(RODBC)  
  ch<-odbcConnect('winddata',uid='root',pwd='123')
  sqlbase_hs300<-paste("select datetime,close from index1 where datetime>='2005-01-01'")
  hs300<-sqlQuery(ch,sqlbase_hs300)
  row<- c('2004-12-31',1000)
  hs300<-rbind(row,hs300)
  pct_chg_1<-NULL
  pct_chg<-NULL
  mixed<-NULL
  mixed<-c(mixed,1)
  
  hs300_20<-NULL
  i=1
  while (i<length(nv))
  {
    mixed<-c(mixed,1)
    pct_chg_1[i]=nv_1[i+1]/nv_1[i]
    pct_chg[i]=nv[i+1]/nv[i]
    i=i+1
  }

  k=1
  kk=1
  ttt=0
  ii=1
  close=as.numeric(hs300[,2])
  
  while (ii<length(nv))
  { ii=ii+1
    mixed[ii]=mixed[ii-1]*pct_chg[ii-1]
    if(kk<=20)
    {
      hs300_20<-c(hs300_20,close[ii])
    }
    hs300_20=as.numeric(hs300_20)
    if((max(hs300_20[1:kk])-close[ii])/max(hs300_20[1:kk])>0.05) 
    {
      mixed[ii+1]=mixed[ii]*pct_chg_1[ii]
      ii=ii+1
      ttt=ttt+1
    }
    kk=kk+1
    if(kk>20)
    {
      kk=20
      hs300_20<-hs300_20[-1]
    }
    
  }
  if(length(mixed)>length(nv)){
    mixed<-mixed[-length(mixed)]
  }
  datetime=hs300[,1]
  datetime=as.character(datetime)
  datetime=as.Date(datetime)
  mixed=as.data.frame(mixed)
  output=cbind(datetime,mixed)
  colnames(output)=c("datetime","net_value")
  odbcClose(ch)
  return(output)
}
