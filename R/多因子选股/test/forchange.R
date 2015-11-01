library(RODBC)
ch<-odbcConnect('winddata',uid='root',pwd='123')
chg<-chg_date[TestPeriod]
chg2<-chg_date[TradePeriod]

for(i in 1:length(chg)){
  if (chg[i]<"2011-03-28"){
    chg[i]<-'2011-03-28'
  }
}

for(i in 1:length(chg2)){
  if (chg2[i]<"2011-03-28"){
    chg2[i]<-'2011-03-28'
  }
}



paste("select datetime,trade_code, weight from zz500weight where datetime=","'",chg,"'",sep="")
trade_day<-sqlQuery(ch,'select distinct datetime from dailyprice')