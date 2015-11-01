Signal_Dynamic_beat<-function(threshold){
  library(RODBC)  
  ch<-odbcConnect('winddata',uid='root',pwd='123')
  sqlbase_hs300<-paste("select datetime,close from index2 where datetime>='2008-09-17'")
  hs300<-sqlQuery(ch,sqlbase_hs300)
  close=as.numeric(hs300[,2])
  hs300_20<-NULL
  i=1
  while(i<=20)
 {
   hs300_20<-c(hs300_20,close[length(close)-i+1])
   i=i+1
 }
  #threshold=0.05
  signal=0
  if((max(hs300_20[1:20])-close[length(close)])/max(hs300_20[1:20])>threshold)
  {
    signal=1
  }
  
 if(signal==1)
 {
   print("to change!")
 }
  return(signal)
}


