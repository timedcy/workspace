
#############计算momentum##################
library(WindR)
library(RODBC)
route1<-"[JYDB].[dbo]."
ch<-odbcConnect('JYDB',uid='jydb',pwd='jydb')
ch2<-odbcConnect('winddata',uid='root',pwd='123')
get_momentum<-function(datetime,trade_day_chr,trade_codes){
  n<-length(trade_codes)
  p<-which(trade_day_chr==datetime)
  datetime1<-trade_day_chr[p-520]
  datetime2<-datetime
  aaa<-data.frame("TradingDay"=rep(as.character(datetime),n),"secucode"=rep(0,n),"momentum"=rep(0,n))
  for(j in 1:n){
    temp<-trade_codes[j]
    sql_temp<-paste("select b.SecuCode,a.TradingDay,a.OpenPrice,a.ClosePrice from ",route1,"QT_DailyQuote a, ",route1,"SecuMain b where a.InnerCode = b.InnerCode  and b.SecuCategory=1 and a.TradingDay>=\'",datetime1,"\' and a.TradingDay<=\'",datetime2,"\' and b.SecuCode ='",temp,"' order by a.TradingDay desc",sep="")
    data<-sqlQuery(ch,sql_temp)
    if(nrow(data)>0){
      data[which(data[,c(3,4)]==0),c(3,4)]<-NA
      #   data<-data[complete.cases(data),]
      ret<-data[1:(nrow(data)-20),4]/data[21:nrow(data),3]-1
      T<-length(ret)
      weight_seq<-c(1:T)
      weight_seq[which(is.na(ret))]<-0
      ret[which(is.na(ret))]<-0
      
      
      w<-0.5^(weight_seq/120)/sum(0.5^(weight_seq/120))
      aaa[j,2:3]<-c(temp,sum(w*log(1+ret)))
    }else{aaa[j,2:3]<-c(temp,NA)}}
  aaa[which(aaa[,3]>1000),3]<-NA
  aaa<-aaa[complete.cases(aaa),]
  aaa[,3]<-as.numeric(aaa[,3])
  aaa[,3]<-round(aaa[,3],4)
  #   aaa[,3]<-rep(as.character(datetime),nrow(aaa))
  #   colnames(aaa[,3])<-"TradingDay"
  return(aaa)
}


add_zero_for_secucode<-function(data){
  m<-c('0','00','000','0000','00000')
  temp<-c()
  for(i in 1:length(data)){
    code<-data[i]
    code<-as.character(code)
    p<-6-length(strsplit(code,'')[[1]])
    code<-paste(m[p],code,sep='')
    temp[i]<-code
  }
  return(temp)
}

sql_tradeday<-paste("select distinct TradingDay from ",route1,"QT_DailyQuote where TradingDay > '2003-01-02' ORDER BY TradingDay")
trade_day<-sqlQuery(ch,sql_tradeday)[,1]
#trade_day<-as.Date(trade_day)
trade_day_chr<-as.character(trade_day)
end<-which(trade_day_chr=='2015-07-17')
chg_num<-seq(1816,end,1) # chg_num<-seq(619,3088,20) the best period #设置起止日期（如数据更新，需更改区间）
chg_date<-trade_day_chr[chg_num] #换仓日赋值
trade_day_chr<<-trade_day_chr
momen<-list()
for(i in 1789:1809){
  datetime1<-trade_day_chr[i-21]
  datetime2<-trade_day_chr[i]
  sql_temp<-paste("select distinct b.SecuCode from ",route1,"QT_DailyQuote a, ",route1,"SecuMain b where a.InnerCode = b.InnerCode  and b.SecuCategory=1 and a.TradingDay=\'",datetime1,"\' and a.OpenPrice>0 order by b.SecuCode",sep="")
  temp<-sqlQuery(ch,sql_temp)
  temp[,1]<-add_zero_for_secucode(temp[,1])
  sql_temp<-paste("select distinct b.SecuCode from ",route1,"QT_DailyQuote a, ",route1,"SecuMain b where a.InnerCode = b.InnerCode  and b.SecuCategory=1 and a.TradingDay=\'",datetime2,"\' and a.OpenPrice>0 order by b.SecuCode",sep="")
  temp1<-sqlQuery(ch,sql_temp)
  temp1[,1]<-add_zero_for_secucode(temp1[,1])
  tempcombine<-c()
  k<-1
  for(j in 1:nrow(temp)){
    if(temp[j,1]%in%temp1[,1]){
      tempcombine[k]<-as.character(temp[j,])
      k<-k+1
    }
  }
  trade_codes<-add_zero_for_secucode(tempcombine)
  momen[[i-1788]]<-get_momentum(datetime2,trade_day_chr,trade_codes)
  print(i)
}

Sys.time()
save(momen,file = "Z:/team4share/策略开发/multi_factor-style_neutral/all A/momentum1789-1809.Rdata")


###################存储momentum到wind数据库

library(RODBC)
ch2<-odbcConnect('winddata',uid='root',pwd='123')
load("F:/desktop/all A/momentum1645-1900.Rdata")
for(i in 1645:1750){
  #momen[[i-1644]][,3]<-rep(trade_day_chr[i],nrow(momen[[i-1644]]))
  colnames(momen[[i-1644]])[3]<-"TradingDay"
  llll<-momen[[i-1644]][,3]
  momen[[i-1644]][,3]<-momen[[i-1644]][,2]
  momen[[i-1644]][,2]<-momen[[i-1644]][,1]
  momen[[i-1644]][,1]<-llll
}
for(i in 1645:1750){
  colnames(momen[[i-1644]])<-c("TradingDay","secucode","momentum")
}

for(i in 1:length(momen))
{
  momen[[i]][,1]<-as.character(momen[[i]][,1])
}



ch2<-odbcConnect('winddata',uid='root',pwd='123')
for(mm in 1:1456){
  for(nn in 1:nrow(KKK[[mm]])){
    sqlQuery(ch2,paste('insert into momentum_rstr (',paste(colnames(KKK[[1]]),collapse = ',') ,') values (\'',paste(KKK[[mm]][nn,],collapse = '\',\''),'\')' ,sep=''))        
  }