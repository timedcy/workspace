dyna_hedge=function(caplist,stock_pick,beta,impact.ratio,date,zz500ic,threshold) {
  caplist2=Backtesthedge5(30000000,0.85,beta,stock_pick,0.00035,0.00003,0.001,impact.ratio,0.00017,date,0.05,zz500ic)[[1]]
  nv_1=caplist2[,2]/caplist2[1,2]
  nv=caplist[,2]/caplist[1,2]
  library(RODBC)  
  ch<-odbcConnect('winddata',uid='root',pwd='123')
  sqlbase_hs300<-paste("select datetime,close from index2 where datetime>='2008-09-17'")
  hs300<-sqlQuery(ch,sqlbase_hs300)
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
  
  while (ii<length(nv))
  { ii=ii+1
    mixed[ii]=mixed[ii-1]*pct_chg[ii-1]
    if(kk<=20)
    {
      hs300_20<-c(hs300_20,hs300[ii,2])
    }
   
    if((max(hs300_20[1:kk])-hs300[ii,2])/max(hs300_20[1:kk])>threshold) 
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
  
  datetime=hs300[,1]
  datetime=as.character(datetime)
  datetime=as.Date(datetime)
  mixed=as.data.frame(mixed)
  output=cbind(datetime,mixed)
  colnames(output)=c("datetime","net_value")
  odbcClose(ch)
  return(output)
}
