#Copyright by zdqh_invest
#Author:wang gaobin<313085945@qq.com>
#File description:this function select stock by month effect and amt effect to 
#                 construct the portfolio during the starttime and its following period's month.

PortfolioConstruct<-function(mylist,select_industry,start_time="2005-01-01",period=124,yuzhi=6,bili=0.5,pick="hs300weight") {
  #construct portfolio based on select_industry and mylist from pick
  #
  #Args:
  #    mylist:allA's average amt and average mkt_freeshares of month level 
  #    select_industry:seleted industry every month based on month effect
  #    start_time: the portfolio's start time 
  #    period:the portfolio's period
  #    yuzhi:a integer,we select industry by yuzhi
  #    bili:a numeric between 0~1,we select stock pick with amt effect
  #    pick:a stock pick ,from which we select stock
  #
  #Returns:
  #       final_stock_pick:our portfolio
  library(RODBC)
  ch<-odbcConnect("winddata","root","123")
  indexdate<-sqlQuery(ch,paste("select distinct datetime from",pick))[,1]
  indexdate<-as.Date(indexdate)
  
  select_industry=select_industry[which(select_industry$freq>yuzhi),]
  final_stock_pick<-list()
  date<-seq(as.Date(start_time), length=period, by="month")
  date<-as.Date(date)
  for (i in 1:length(date)) {
      thisdate<-date[i]
      minindexdate<-min(indexdate)
      ##select stock pick in date[i]
     if (thisdate<=minindexdate) { 
       thisindexdate<-minindexdate } else { 
         thisindexdate<-max(indexdate[indexdate<thisdate])}
    thispick<-sqlQuery(ch,paste("select trade_code,weight from",pick,"where datetime='",thisindexdate,"'"))
    
    ##to simple the process ,we let thishs300<-thispick. but the pick is not necessarily  hs300
    thishs300<-thispick
    colnames(thishs300)<-c("trade_code","weight_hs300")
    thishs300[,"weight_hs300"]<- thishs300[,"weight_hs300"]/sum( thishs300[,"weight_hs300"])
    
    ###select allA in date[i]
    thisallA<-mylist[[i]]
    
    ##select industry in date[i]
    thismonth<-as.POSIXlt(thisdate)$mon+1
    thisindustry<-select_industry[which(select_industry$month==thismonth),]
    thisindustry<-thisindustry[,"industry"]
    thisindustry<-as.data.frame(thisindustry)
    colnames(thisindustry)<-"industry"
    
    ###select hs300's stock in selected industry by month effect
    thisstock_month<-merge(thisindustry,thisallA,by="industry")
    thisstock_month<-merge(thishs300,thisstock_month,by="trade_code")
    
    ##select stock  with month and amt effect
    before_sort_data<-thisstock_month
    after_sort_data<-before_sort_data[order(before_sort_data[,"avg_amt"]),]
    after_sort_data<-after_sort_data[is.na(after_sort_data[,"avg_amt"])!=1,]
    a<-dim(after_sort_data)[1];
    a1<-a*bili;
    a1<-floor(a1);
    select_sort_data1<-after_sort_data[1:a1,];  ###selected trade_code in i_th month
    thisstock_month_amt<-as.data.frame(select_sort_data1)
    
    ##caculate the weight 
    temp<-thisstock_month_amt
    industry_weight<-aggregate(x=temp$weight_hs300,by=list(temp$industry),FUN=sum) 
    colnames(industry_weight)<-c("industry","weight_industry")
    industry_weight[,"weight_industry"]<- industry_weight[,"weight_industry"]/sum(industry_weight[,"weight_industry"])
    temp<-merge(temp,industry_weight,by="industry")
    
    industry_mkt<-aggregate(x=temp$avg_mkt,by=list(temp$industry),FUN=sum) 
    colnames(industry_mkt)<-c("industry","mkt_industry")
    temp<-merge(temp,industry_mkt,by="industry")
    temp[,"weight_mkt"]<-temp[,"avg_mkt"]/temp[,"mkt_industry"]
    
    temp[,"weight"]<-temp[,"weight_industry"]*temp[,"weight_mkt"]
    
    thisstock_month_amt<-temp[,c("trade_code","weight")]

    final_stock_pick[[i]]<-thisstock_month_amt
   
  }
 trade_day<-sqlQuery(ch,'select distinct datetime from dailyprice')[,1]
 trade_day<-as.Date(trade_day)
 trad<-NULL;
 for ( i in date){
   a<-max(trade_day[trade_day<i])
   trad<-c(trad,a)
 }
 trad<-as.Date(trad,origin="1970-01-01")
 trad<-as.character(trad)
 names(final_stock_pick)<-trad
 odbcClose(ch)
 return(final_stock_pick)
}


