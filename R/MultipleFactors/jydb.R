setwd("z:/jydb")

#install.packages("dplyr")

library(RODBC)
#library(dplyr)


ch<-odbcConnect('JYDB.dbo',uid='jydb',pwd='jydb')
ch2<-odbcConnect('winddata',uid='root',pwd='123')

load("Stock_code.RData")
sql_tradeday<-paste("select distinct TradingDay from [dbo].qt_dailyquote where TradingDay > '2003-01-02' ORDER BY TradingDay")
trade_day<-sqlQuery(ch,sql_tradeday)[,1]
#trade_day<-as.Date(trade_day)
trade_day_chr<-as.character(trade_day)

chg_num<-seq(1753,3088,20) # chg_num<-seq(900,2940,20) the best period #设置起止日期（如数据更新，需更改区间）
chg_date<-trade_day_chr[chg_num[1:(length(chg_num)-1)]] #换仓日赋值

#for size-INCAP

index=1306

i=length(chg_date)
date<-chg_date[i]
trade_codes<-Stock_code[index]
trade_codes<-unlist(trade_codes)
names(trade_codes)<-NULL
matrix(trade_codes,nrow=1)

#  待改 size-INCAP
for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.ClosePrice, a.TotalShare from QT_MonthData a, secumain b where a.InnerCode = b.InnerCode  and b.SecuCategory=1 and b.secucode = '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  if(nrow(data)>0){
    #colnames(data)<-tolower(colnames(data))
    data[,1]<-as.character(data[,1])
    data2<-data
    data2[is.na(data2)] <- 0
    data3<-data2
    data3[,3]<-data2[,3]*data2[,4]
    data3<-data3[,-4]
    names(data3)[3]='totalmv'
    data3[,3]=round(log(data3[,3]),2)
    #sqlQuery(ch2,paste('insert into size (',paste(colnames(data),collapse = ',') ,') values (\'',paste(data[1,],collapse = '\',\''),'\')' ,sep=''))        
    for(jj in 1:nrow(data3)){
      sqlQuery(ch2,paste('insert into size (',paste(colnames(data3),collapse = ',') ,') values (\'',paste(data3[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
  }
  
}

# for growth1 OperatingRevenueGrowRate 营业收入同比增长（%），AvgNPYOYPastFiveYear 过去五年同期归属母公司净利润平均增幅（%）

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.OperatingRevenueGrowRate,a.AvgNPYOYPastFiveYear from LC_MainIndexNew a, secumain b where a.CompanyCode = b.CompanyCode and b.SecuCategory=1 and b.secucode = '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  #colnames(data)<-tolower(colnames(data))
  if(nrow(data)>0){
    data[,1]<-as.character(data[,1])
    data2<-data
    data2[is.na(data2)] <- 0
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into growth1 (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
    
  }
  
}

#for growht2 NPYOYConsistentForecast  一致预期净利润增幅（%）

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.InfoPublDate,a.EndDate,b.secucode,a.NPYOYConsistentForecast from LC_PerformanceForecast a, secumain b where a.CompanyCode = b.CompanyCode and b.SecuCategory=1 and b.secucode =  '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  if(nrow(data)>0){
    #colnames(data)<-tolower(colnames(data))
    data[,1]<-as.character(data[,1])
    data[,2]<-as.character(data[,2])
    
    data2<-data
    data2[is.na(data2)] <- 0
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into growth2 (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
  }
  
}



#for Earnings Yield EPIBS

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.InfoPublDate,b.secucode,a.EEPSFloor,c.ClosePrice from LC_PerformanceForecast a, secumain b, qt_dailyquote c where a.CompanyCode = b.CompanyCode and a.InfoPublDate=c.TradingDay and b.SecuCategory=1 and b.InnerCode=c.InnerCode and b.secucode =   '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  if(nrow(data)>0){
    data[,1]<-as.character(data[,1])
    #data[,2]<-as.character(data[,2])
    data2<-data
    data2[is.na(data2)] <- 0
    data3<-data2
    data3[,3]<-data2[,3]/data2[,4]
    data3<-data3[,-4]
    names(data3)[3]='epibs'
    for(jj in 1:nrow(data3)){
      sqlQuery(ch2,paste('insert into ey_epibs (',paste(colnames(data3),collapse = ',') ,') values (\'',paste(data3[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
  }
  
}

#for Earnings Yield EP,EP=1/PE
for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.PE from LC_IndicesForValuation a, secumain b where a.InnerCode = b.InnerCode and b.SecuCategory=1 and b.secucode =   '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  if(nrow(data)>0){
    #colnames(data)<-tolower(colnames(data))
    data[,1]<-as.character(data[,1])
    #data[,2]<-as.character(data[,2])
    data2<-data
    data2[is.na(data2)] <- 0
    data2[,3]<-1/data2[,3]
    
    #data3<-data2
    #data3[,3]<-data2[,3]/data2[,4]
    #data3<-data3[,-4]
    names(data2)[3]='ep'
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into ey_ep (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
  }
  
}


#for Earnings Yield CETOP, CashFlowPS/close

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.CashFlowPS,c.ClosePrice from LC_MainIndexNew a, secumain b, qt_dailyquote c where a.CompanyCode = b.CompanyCode and b.SecuCategory=1 and a.EndDate=c.TradingDay and b.InnerCode=c.InnerCode and b.secucode =   '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  if(nrow(data)>0){
    #colnames(data)<-tolower(colnames(data))
    data[,1]<-as.character(data[,1])
    #data[,2]<-as.character(data[,2])
    data2<-data
    data2[is.na(data2)] <- 0
    data3<-data2
    data3[,3]<-data2[,3]/data2[,4]
    data3<-data3[,-4]
    names(data3)[3]='cetop'
    for(jj in 1:nrow(data3)){
      sqlQuery(ch2,paste('insert into ey_cetop (',paste(colnames(data3),collapse = ',') ,') values (\'',paste(data3[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
    
  }
  
}





# for Value-BTOP

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.InfoPublDate,b.secucode,a.SEWithoutMI,a.TotalShares,c.ClosePrice from LC_MainDataNew a, secumain b, qt_dailyquote c where a.CompanyCode = b.CompanyCode and b.SecuCategory=1 and a.EndDate=c.TradingDay and b.InnerCode=c.InnerCode and b.secucode =  '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  if(nrow(data)>0){
    #colnames(data)<-tolower(colnames(data))
    data[,1]<-as.character(data[,1])
    #data[,2]<-as.character(data[,2])
    data2<-data
    data2[is.na(data2)] <- 0
    data3<-data2
    data3[,3]<-(data2[,3])/(data2[,4]*data2[,5])
    data3<-data3[,-5]
    data3<-data3[,-4]
    names(data3)[3]='btop'
    for(jj in 1:nrow(data3)){
      sqlQuery(ch2,paste('insert into value (',paste(colnames(data3),collapse = ',') ,') values (\'',paste(data3[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
  } 
  
}



#for leverage1 





#for Leverage_dtoa

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.DebtToAssetValue1 from LC_WorkandGrowIndex a, secumain b where a.CompanyCode = b.CompanyCode and b.SecuCategory=1 and b.secucode = '",temp,"'",sep="")
  data<-sqlQuery(ch,sql_temp)
  #colnames(data)<-tolower(colnames(data))
  if(nrow(data)>0){
    data[,1]<-as.character(data[,1])
    data2<-data
    data2[is.na(data2)] <- 0
    names(data2)[3]='dtoa'
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into leverage_dtoa (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
      
    }
    
  }
  
}

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

#for Liquidity STOM  另外两个没做。错误 因为第二个sql语句出现停牌无值的情况  改好 未测试。

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.FloatShare from QT_MonthData a, secumain b where a.InnerCode = b.InnerCode and b.SecuCategory=1 and a.EndDate>'2009-12-01' and b.secucode =  '",temp,"'order by a.EndDate",sep="")
  data<-sqlQuery(ch,sql_temp)
  #data[is.na(data)] <- 0
  data<-data[complete.cases(data),] 
  data[,1]<-as.character(data[,1])
  data2<-data
  p=nrow(data2)
  m=1
  while(m<p){
    if(data2[m,1]==data2[m+1,1]){
      data2<-data2[-(m+1),]
    }
    p=p=nrow(data2)
    m=m+1
  }
  data2<-data.frame(data2,stom=c(NA))
  if(nrow(data2)>20){
    for(k in 1:(nrow(data2)-1)){
      floor_date<-data2[k,1]
      ceil_date<-data2[k+1,1]
      sql_temp2<-paste("select a.TurnoverVolume from qt_dailyquote a, secumain b where a.innercode = b.innercode and b.SecuCategory=1 and a.TradingDay> '",floor_date,"' and a.TradingDay< '",ceil_date,"' and b.secucode =  '",temp,"'",sep="")
      daily_data<-sqlQuery(ch,sql_temp2)
      if(nrow(daily_data)>0){
        sum_daily<-sum(daily_data[,1])
        data2[k+1,4]<-sum_daily/data[k+1,3]
      }
    }
    data2[,4]=round(log(data2[,4]),2)
    data2[data2==-Inf]<-0
    data2<-data2[complete.cases(data2),] 
    #data2[is.na(data2)] <- 0
    #data3<-filter(data2, FloatShare !='0')
    #names(data2)[3]='stom'
    #data2<-data2[-1,]
    data2<-data2[,-3]
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into liquidity (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
    }
  }
  
}


#for volatility cmra   #，利用当日开盘价与流通股本乘积算涨跌幅  疑似改好

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.EndDate,b.secucode,a.ClosePrice,a.FloatShare from QT_MonthData a, secumain b where a.InnerCode = b.InnerCode and b.SecuCategory=1 and a.EndDate>'2008-12-01' and b.secucode =  '",temp,"'order by a.EndDate",sep="")
  data<-sqlQuery(ch,sql_temp)
  data[is.na(data)] <- 0
  data[,1]<-as.character(data[,1])
  data2<-data
  data2<-data.frame(data2,yield=c(NA))
  if(nrow(data)>1){
    for(k in 2:nrow(data2)){
      p2=data2[k,3]*data2[k,4]
      p1=data2[k-1,3]*data2[k-1,4]
      data2[k,5]<-round(log(p2/p1),2)
    }
    data2<-data2[-1,]
    data2<-data.frame(data2,cmra=c(NA))
    for(m in 1:nrow(data2)){
      if(m>12){
        o=m-12
        a=max(data2[o:m,5])
        b=min(data2[o:m,5])
        crma=log(1+a)-log(1+b)
        data2[m,6]<-crma
      }
    }
    data2<-data2[complete.cases(data2),] 
    data2<-data2[,-3]
    data2<-data2[,-3]
    data2<-data2[,-3]
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into volatility_cmra (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
    }
  }
}

#for volatility dastd  日度数据，需处理

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.TradingDay,b.secucode,a.OpenPrice,a.ClosePrice from QT_DailyQuote a, secumain b where a.InnerCode = b.InnerCode  and a.TradingDay>'2008-12-01' and b.SecuCategory=1 and b.secucode =  '",temp,"'order by a.TradingDay",sep="")
  data<-sqlQuery(ch,sql_temp)
  data[is.na(data)] <- 0
  data[,1]<-as.character(data[,1])
  data2<-data
  data2<-data.frame(data2,yield=c(NA))
  if(nrow(data)>0){
    for(k in 1:nrow(data2)){
      data2[k,5]<-data2[k,4]/data2[k,3]-1
    }
    data2<-data.frame(data2,dastd=c(NA))
    data2[data2==Inf]<-0
    for(m in 1:nrow(data2)){
      if(m>250){
        o=m-250
        dastd=sd(data2[o:m,5],na.rm=TRUE)
        data2[m,6]<-dastd
      }
    }
    data2<-data2[complete.cases(data2),] 
    data2<-data2[,-3]
    data2<-data2[,-3]
    data2<-data2[,-3]
    for(jj in 1:nrow(data2)){
      sqlQuery(ch2,paste('insert into volatility_dastd (',paste(colnames(data2),collapse = ',') ,') values (\'',paste(data2[jj,],collapse = '\',\''),'\')' ,sep=''))        
    }
  }  
}

#for Beta and volatility-hsigma  待测试， 日度数据  j=9  所有表全部清空再入一遍  证券类别等于1，于HS300长度对齐，若上市日期大于2008-12-01

for(j in 1:length(trade_codes)){
  temp<-substr(trade_codes[j],1,6)
  sql_temp<-paste("select a.TradingDay,b.secucode,a.OpenPrice,a.ClosePrice from QT_DailyQuote a, secumain b where a.InnerCode = b.InnerCode  and b.SecuCategory=1 and a.TradingDay>'2008-12-01' and b.secucode =  '",temp,"'order by a.TradingDay",sep="")
  data<-sqlQuery(ch,sql_temp)
  data[is.na(data)] <- 0
  data[,1]<-as.character(data[,1])
  data2<-data
  data2<-data.frame(data2,yield=c(NA))
  if(nrow(data)>0){
    for(k in 1:nrow(data2)){
      data2[k,5]<-data2[k,4]/data2[k,3]-1
    }
    data2[data2==Inf]<-0
    sql_temp2<-paste("select datetime, pct_chg from index1 where datetime >'2008-12-01'")
    hs300<-sqlQuery(ch2,sql_temp2)
    while(nrow(hs300)>nrow(data2)){
      hs300<-hs300[-1,]
    }
    data3<-cbind(data2,hs300)
    data3<-data.frame(data3,beta=c(NA))
    data3<-data.frame(data3,residuals=c(NA))
    for(m in 1:nrow(data3)){
      if(m==251){
        m=251
        result<-lm(yield~pct_chg,data3[1:m,])
        res<-result[2]
        res<-res[[1]]
        for(n in 1:250){
          data3[n,9]=res[n]
        }
      }
      if(m>250){
        #m=251
        o=m-250
        #dastd=sd(data2[o:m,5],na.rm=TRUE)
        #data2[m,6]<-dastd
        result<-lm(yield~pct_chg,data3[o:m,])
        res<-result[2]
        res<-res[[1]]
        beta<-coefficients(result)[2]
        beta<-as.numeric(beta)
        data3[m,8]=beta
        data3[m,9]=res[length(res)]
      }
    }
    data3<-data.frame(data3,hsigma=c(NA))
    for(m in 1:nrow(data3)){
      if(m>250){
        o=m-250
        hsigma=sd(data2[o:m,5],na.rm=TRUE)
        data3[m,10]<-hsigma
      }
    }
    data3<-data3[complete.cases(data3),] 
    data4<-data3[,c(1,2,8)]
    data5<-data3[,c(1,2,10)]
    for(jj in 1:nrow(data4)){
      sqlQuery(ch2,paste('insert into beta (',paste(colnames(data4),collapse = ',') ,') values (\'',paste(data4[jj,],collapse = '\',\''),'\')' ,sep=''))        
    }
    for(jj in 1:nrow(data5)){
      sqlQuery(ch2,paste('insert into hsigma (',paste(colnames(data5),collapse = ',') ,') values (\'',paste(data5[jj,],collapse = '\',\''),'\')' ,sep=''))        
    }
  }  
}


#