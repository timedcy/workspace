setwd("C:/Users/liuli/workspace/MultipleFactors")
library(WindR)
library(RODBC)
route1<-"[JYDB].[dbo]."
ch<-odbcConnect('JYDB',uid='jydb',pwd='jydb')
ch2<-odbcConnect('winddata',uid='root',pwd='123')
load("Stock_code.RData")

sql_tradeday<-paste("select distinct TradingDay from ",route1,"QT_DailyQuote where TradingDay > '2005-01-02' ORDER BY TradingDay")
trade_day<-sqlQuery(ch,sql_tradeday)[,1]
#trade_day<-as.Date(trade_day)
trade_day_chr<-as.character(trade_day)
end<-which(trade_day_chr=='2015-10-09')
chg_num<-seq(1,end,1) # chg_num<-seq(619,3088,20) the best period #设置起止日期（如数据更新，需更改区间）
chg_date<-trade_day_chr[chg_num] #换仓日赋值

i=length(chg_date)
date<-chg_date[i]
trade_codes<-Stock_code[index]
trade_codes<-unlist(trade_codes)
names(trade_codes)<-NULL
matrix(trade_codes,nrow=1)

#for Leverage2   DebtAssetsRatio 资产负债率  LongDebtToEquity  长期负债/股东权益

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.DebtAssetsRatio,a.TotalAssets,a.TotalNonCurrentLiability from LC_BalanceSheetAll a, secumain b where a.CompanyCode = b.CompanyCode and b.SecuCategory=1 and b.secucode =  '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  #colnames(data)<-tolower(colnames(data))
  if(nrow(data)>0){
    data[,1]<-as.character(data[,1])
    data2<-data
    data2[is.na(data2)] <- 0
    names(data2)[3]='dar'
    names(data2)[4]='ldte'
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into leverage2 (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
    
  }
  
}