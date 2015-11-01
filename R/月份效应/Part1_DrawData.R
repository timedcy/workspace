#Copyright by zdqh_invest;
#Author:wang gaobin<313085945@qq.com>
#file describtion:this part is a function which draw data from our mysql database

DrawData<-function(starttime, lengthn,industry) {
  #Draw allA's average amt and mkt_freeshares  of  month level during the starttime and the following length's month
  #
  #Args:
  #    starttime:must be a Date type and comply the pattern of "YYYY-MM-DD"
  #    length:must be a integer
  #    industry:is a data.frame, the first col's name is "trade_code",the second name is "industry"
  #Returns:
  #        this function return "mylist".
  #       "mylist" is a list type, and it contains "length" second level list.
  #        every second level list's name means datetime patterned by "YYYY-MM-DD"
  #        every second level list contain allA's trade_code,average amt ,average mkt_freeshares,and its industry
  if (class(starttime)!="Date")
    stop("starttime must be Date type")
  if (!is.numeric(lengthn))
    stop("lenght must be a numeric type")
    
  library(RODBC)
  ch<-odbcConnect('winddata',uid='root',pwd='123')
  date<-seq(starttime, length=lengthn, by="month")
  date<-as.character(date)
  trad<-date
  mylist<-list()
  # mylist为一个list类型的数据，包含若干个data.frame型的数据；如果该data.frame的名字是“2010-02-01”，
  #则该data.frame为所有A股在2010年1月份的月内平均成交量和流动市值,用于2月份的选股；
  n<-length(trad)-1
  for (j in 1:n) {
    sql1<-paste("SELECT trade_code,avg(amt) as avg_amt,avg(mkt_freeshares) as avg_mkt from winddata.dailyprice where ","datetime>='",trad[j],"'","and datetime< '",trad[j+1],"'"," group by trade_code",sep="")
    avg_amt<-sqlQuery(ch,sql1)
    avg_amt<-as.data.frame(avg_amt)
    avg_amt<-merge(industry,avg_amt,by="trade_code")   ###maybe need to improve 
    mylist[[j]]<-avg_amt
  } 
  names(mylist)<-trad[2:(n+1)]
  odbcClose(ch)
  return(mylist)
}



