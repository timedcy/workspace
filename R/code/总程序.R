setwd("Z:\\实盘策略\\多因子选股模块\\code")
source('f-factorpick.R')
source('f-stockpick.R')


TT<-length(chg_date)
L<-24  #筛选因子的所用数据长度
H<-3   #因子有效期
O<-79   #起点
Os<-seq(O,TT-L+1,H)
stock_pick<-list()
ind_fct<-list()


for (i in Os){
  if(i+L>TT){
    break;
  }
  TestPeriod<-seq(i,i+L-1,1)                 
  TradePeriod<-seq(i+L,min(i+L+H-1,TT),1)    
  for (j in ind_set){
    stock_pool<-intersect(industry[which(industry[,3]==j),1],zz800) #股票池
    if(length(stock_pool)>2){
      chg<-chg_date[TestPeriod]
      aaa<-factorpick1(fct_name,chg,stock_pool)
      if(class(aaa)!='numeric'){
        aaba<-cbind(ind_fct[[j]],aaa[,1])
        colnames(aaba)[ncol(aaba)]<-chg_date[i+H]
        ind_fct[[j]]<-aaba      
        fct<-list()      
        for (jj in chg_date[TradePeriod]){
          fct[[jj]]<-aaa             
        }
        bbb<-stockpick_1(fct,stock_pool,top=0.05) #选股百分比
        for(jjj in names(bbb)){
          aaca<-bbb[[jjj]]
          aaca['industry']<-j
          aaca['weight']<-aaca['weight']*weight[j]
          stock_pick[[jjj]]<-rbind(stock_pick[[jjj]],aaca)
        }
      }           
    }    
  }
  print(i)
  mark<-i
}

for (jjj in names(stock_pick)){
  aacf<-stock_pick[[jjj]]
  aacf['weight']<-aacf['weight']/sum(aacf['weight'])
  stock_pick[[jjj]]<-aacf
} #权重


save(stock_pick,file=paste("Z:\\实盘策略\\多因子选股模块\\RData\\",Sys.Date(),"-zz800-80q",mark,".RData",sep="")) 


# 1.起始资本 2.起始投资比例（默认0.8可能变化） 3.对冲比例 4.每日选股 5.股票手续费 6.期货手续费 7.印花税 8.股票冲击成本 9.期货的冲击成本 10.额外天数
#caplist1<-Backtesthedge(30000000,0.80,0.9,stock_pick,0.00035,0.00003,0.001,0.003,0.00017,30)



