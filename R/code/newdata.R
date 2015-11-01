source("f-getdata.R")
#初始化 已存数据至2015-02-27 
chg_num<-seq(905,3100,20) # chg_num<-seq(900,2940,20) the best period #设置起止日期（如数据更新，需更改区间）
chg_date<-trade_day_chr[chg_num[1:(length(chg_num)-1)]] #换仓日赋值
chg_dateplus<-trade_day_chr[chg_num] #增加一
fctdata<-list()

i=chg_date[length(chg_date)]
for(i in chg_date){  #该循环将数据库表格seasona根据日期分割成多个dataframe
  print(i)
  temp<-get_norm(i,'roe_yearly','seasona') #表格<-交易代码+净资产收益率ROE
  temp<-merge(temp,get_norm(i,'gctogr','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'netprofitmargin','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'grossprofitmargin','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'finaexpensetogr','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'yoyroe','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'yoy_or','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'yoynetprofit_deducted','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'yoynetprofit','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'yoy_assets','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'debttoequity','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'debttoassets','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'longdebttolongcaptial','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'ncatoequity','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'currentdebttodebt','seasona'),by='trade_code',all=TRUE)   
  temp<-merge(temp,get_norm(i,'assetsturn','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'current','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'quick','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_norm(i,'invturn','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_pe(i,'ttm_np_belongto_parcomsh','pe_ttm','seasonttm'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_price(i,'dividendyield2','dailyprice'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_price(i,'annualstdevr_100w','dailyprice'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_pe(i,'eqy_belongto_parcomsh','pb','seasonb'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_price(i,'mkt_cap','dailyprice'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_price(i,'mkt_freeshares','dailyprice'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_pe(i,'ttm_tot_oper_rev','ps','seasonttm'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_pe(i,'ttm_tot_oper_rev','pcf_ocf','seasonttm'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_avg(i,63,'amt'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_avg(i,21,'turn'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_htol(i,21),by='trade_code',all=TRUE)
  temp<-merge(temp,get_chg(i,21),by='trade_code',all=TRUE)
  temp<-merge(temp,get_rsi(i,14),by='trade_code',all=TRUE)
  temp<-merge(temp,get_yoy(i,'invturn','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_yoy(i,'quick','seasona'),by='trade_code',all=TRUE)
  temp<-merge(temp,get_mton(i,'ttm_np_belongto_parcomsh','seasonttm','eqy_belongto_parcomsh','seasonb','roe_ttm'),by='trade_code',all=TRUE)
  temp['ep']<-1/temp['pe_ttm']
  temp<-temp[-which(colnames(temp)=='pe_ttm')]
  fctdata[[i]]<-temp
}  


dailydata<-list()

for (i in chg_dateplus){ #读取dailyprice表中不同日期中三个指标对应的值，并储存
  print(i)
  temp1<-c('close','adjfactor','amt')
  temp<-get_price(i,temp1,'dailyprice')
  dailydata[[i]]<-temp
  
}

fct_name<-colnames(fctdata[[1]])[-1] #因子名除代码 factor names
hs<-sqlQuery(ch,'select datetime,close from index1')  #日期&收盘价
rownames(hs)<-hs[,1] 


industry<-read.csv('industry.csv',stringsAsFactors=F)
ind_set<-unique(industry[,3]) #行业名
for (i in 1:length(ind_set)){
  industry[which(industry[,3]==ind_set[i]),4]<-as.character(i) #增加一列对每只股对应的行业进行标注（根据存储在ind_set中的序号）
}

aaaaa<-read.csv('000300closeweight.csv',stringsAsFactors=F)  #某天沪深300收盘后比例
rownames(aaaaa)<-aaaaa[,1]


#计算沪深300某天各行业的权重
weight<-rep(0,length(ind_set))
names(weight)<-ind_set  #初始化各行业权重赋值为0
for (i in aaaaa[,1]){
  abc<-industry[which(industry[,1]==i),3]
  
  weight[abc]<-weight[abc]+aaaaa[i,2]/100
}

#中证800股票代码
zz800<-read.csv('zz800.csv',stringsAsFactors=F)
zz800<-zz800[,1]
#沪深300股票代码
zz300<-read.csv('zz300.csv',stringsAsFactors=F)
zz300<-zz300[,1]
#股票代码
zz2000<-as.character(industry[,1])

hs300<-hs[trade_day_chr[chg_num],] #特定区间内的日期跟收盘价
yield_hs<-diff(hs300[,2])/hs300[1:(nrow(hs300)-1),2] #每期增长率[第二期收盘价-第一期）/第一期]
names(yield_hs)<-hs300[1:(nrow(hs300)-1),1]



