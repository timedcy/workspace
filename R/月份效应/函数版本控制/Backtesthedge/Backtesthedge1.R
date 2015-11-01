#本回测再也不会买到停牌的股票了
#修复只有period.b的情况下会出现period.a的日期的BUG
#注意：本回测同一只股票在同一天不能出现两个数值，否则会因为重复计算而导严重的估值错误
#注意：本回测测试期间绝对不能超过数据库时间范围
Backtesthedge1<-function(cap,Invr,beta,stock,fee.ratio1,fee.ratio2,tax.ratio,impact.ratio1,impact.ratio2,plusday){
  
  library(RODBC)
  ch<-odbcConnect('winddata',uid='root',pwd='123')
  
  caplist<-data.frame()
  
  trade_all<-sqlQuery(ch,'select distinct datetime from dailyprice')[,1]
  trade_all<-as.character(trade_all)
  
  adjday<-names(stock)
  adjday00<-as.Date(adjday,origin='1899-12-30')
  lastday<-as.character(adjday00[length(adjday00)]+plusday)
  
  trad<-NULL
  
  for ( i in adjday){
    trad<-c(trad,min(trade_all[trade_all>i]))
  }
  period<-sqlQuery(ch,paste("select distinct datetime from dailyprice where datetime between '",min(trad),"' and '",lastday,"'") )[,1]
  period<-as.character(period)
  
  period.a<-period[period<'2010-04-17']
  period.b<-setdiff(period,period.a)
  
  caplist<-data.frame()
  caplist[1,'datetime']<-adjday[1]
  caplist[1,'cap']<-cap
  caplist[1,'return']<-0
  caplist[1,'cash']<-cap
  caplist[1,'stockcap']<-0
  caplist[1,'IFcap']<-0
  caplist[1,'lowestcap']<-cap
  
  sql1<-paste("SELECT trade_code,open from winddata.dailyprice WHERE datetime ='",trad[1],"'")
  sql2<-paste("SELECT trade_code,close from dailyprice as a right join (SELECT max(datetime) as da from dailyprice where datetime<'",trad[1],"') as b on a.datetime=b.da")
  sql3<-paste("SELECT trade_code,adjfactor from dailyprice as a right join (SELECT max(datetime) as da from dailyprice where datetime<'",trad[1],"') as b on a.datetime=b.da")
  adjbase<-sqlQuery(ch,sql3)
  stock.open<-sqlQuery(ch,sql1)    
  stock.close<-sqlQuery(ch,sql2)
  
  
  hold.new<-stock[[adjday[1]]]
  hold.new<-merge(hold.new,stock.open,by='trade_code')
  hold.new<-merge(hold.new,stock.close,by='trade_code')
  
  sql4<-paste("SELECT trade_code,amt from winddata.dailyprice WHERE datetime ='",trad[1],"'")
  stock.amt<-sqlQuery(ch,sql4)
  hold.new<-merge(hold.new,stock.amt,by='trade_code')  
  add<-hold.new[is.na(hold.new[,'amt']),]
  add<-add[which(add[,'amt']>0),]
  addw<-0
  if (nrow(add)>0){
    addw<-sum(add[,'weight'])/(nrow(hold.new)-nrow(add))
  }
  hold.new[,'weight']<-hold.new[,'weight']+addw
  hold.new<-hold.new[is.na(hold.new[,'amt'])!=1,]
  hold.new<-hold.new[which(hold.new[,'amt']>0),]
  CT<-hold.new
  
  
  cash<-cap
  
  CT<-CT[is.na(CT[,1])!=1,]
  CT[is.na(CT)]<-0
  CT[,'holdnew']<-floor((cap*Invr*CT[,'weight'])/CT[,'close']/100)
  CT[,'change']<-CT[,'holdnew']
  CT[,'fee']<-abs(CT[,'change']*CT[,'open'])*100*fee.ratio1
  CT[CT$fee<5,'fee']<-5
  CT[CT$change<0,'tax']<-abs(CT[CT$change<0,'change'])*CT[CT$change<0,'open']*100*tax.ratio
  CT[,'impact']<-abs(CT[,'change']*CT[,'open'])*100*impact.ratio1
  CT<-unique(CT)
  
  hold.old<-CT[is.na(CT$open)!=1,c('trade_code','holdnew')]
  colnames(hold.old)<-c('trade_code','holdold')
  
  cost<-sum(CT[,c('fee','tax','impact')],na.rm=TRUE)
  cash<-cash+sum(CT[,'change']*CT[,'open']*100*(-1),na.rm=TRUE)-cost
  stockcap<-sum(CT[is.na(CT$open)!=1,'holdnew'] * CT[is.na(CT$open)!=1,'open'])*100
  cap<-cash+stockcap
  
  stockcost<-cost
  
  #######
  #首先判断测试期是否包含没有股指期货的时间段
  #没有包含则用HS300指数对冲
  
  if (trad[1]<'2010-4-17'){
    
    sqlbase<-paste("select trade_code,open from index1 where datetime='",trad[1],"'")
    futures<-sqlQuery(ch,sqlbase)
    sqlbase2<-paste("SELECT trade_code,close from index1 as a right join (SELECT max(datetime) as da from index1 where datetime<'",trad[1],"') as b on a.datetime=b.da")
    futures2<-sqlQuery(ch,sqlbase2)
    sql3<-paste("SELECT trade_code,adjfactor from dailyprice where datetime='",trad[1],"'")
    stock.adj<-sqlQuery(ch,sql3)
    
    hedge<-data.frame()
    
    hedge[1,'trade_code']<-as.character(futures[,'trade_code'])
    hedge[1,'hold']<-(-1)*floor(stockcap*beta/300/futures[,'open'])
    
    IFfee<-abs(hedge[1,'hold'])*fee.ratio2*futures[ ,'open']*300
    IFimpact<-abs(hedge[1,'hold'])*impact.ratio2*futures[ ,'open']*300
    
    hedge[1,'cost']<-IFfee+IFimpact
    hedge[1,'IFcap']<-hedge[1,'hold']*futures[ ,'open']*300
    
    # IFcash<-(-1)*hedge[1,'hold']*futures[ ,'open']*300-hedge[,'cost'] 
    adjbase<-stock.adj
    cash<-cash-IFfee-IFimpact
    
    #######
    for ( d in 2:length(period.a)){
      
      sql1<-paste("SELECT trade_code,open from winddata.dailyprice WHERE datetime ='",period.a[d],"'")
      stock.open<-sqlQuery(ch,sql1)
      sql2<-paste("SELECT trade_code,close from dailyprice where datetime='",period.a[d-1],"'")
      stock.close<-sqlQuery(ch,sql2)
      sql3<-paste("SELECT trade_code,adjfactor from dailyprice where datetime='",period.a[d-1],"'")
      stock.adj<-sqlQuery(ch,sql3)
      sql4<-paste("SELECT trade_code,low from dailyprice where datetime='",period.a[d-1],"'")
      stock.low<-sqlQuery(ch,sql4)
      
      sqlbase<-paste("select datetime,trade_code, open from index1 where datetime='",period.a[d],"'")
      futures<-sqlQuery(ch,sqlbase)
      sqlbase2<-paste("SELECT datetime,trade_code, close from index1 where datetime='",period.a[d-1],"'")
      futures2<-sqlQuery(ch,sqlbase2)
      
      
      hedge[d,'trade_code']<-as.character(futures[,'trade_code'])
      
      
      if (is.element(period.a[d],trad)==1){   
        
        hold.new<-stock[[adjday[which(trad==period.a[d])]]]
        
        sql4<-paste("SELECT trade_code,amt from winddata.dailyprice WHERE datetime ='",period.a[d],"'")
        stock.amt<-sqlQuery(ch,sql4)
        hold.new<-merge(hold.new,stock.amt,by='trade_code')  
        add<-hold.new[is.na(hold.new[,'amt']),]
        add<-add[which(add[,'amt']>0),]
        addw<-0
        if (nrow(add)>0){
          addw<-sum(add[,'weight'])/(nrow(hold.new)-nrow(add))
        }
        hold.new[,'weight']<-hold.new[,'weight']+addw
        hold.new<-hold.new[is.na(hold.new[,'amt'])!=1,]
        hold.new<-hold.new[which(hold.new[,'amt']>0),]
    
        #分配资金用昨日收盘，
        #包含股票价格，股票代码的data.frame
        temp<-merge(hold.old,stock.adj,by='trade_code',all.x=TRUE)
        temp<-merge(temp,adjbase,by='trade_code')
        colnames(temp)<-c('trade_code','holdold','adjnew','adjbase')
        temp[,'holdold2']<-floor(temp[,'holdold']*temp[,'adjnew']/temp[,'adjbase'])
        adjbase<-stock.adj
        hold.old<-temp[,c('trade_code','holdold2')]
        colnames(hold.old)<-c('trade_code','holdold')
        
        CT<-NULL
        CT<-merge(hold.old,hold.new,by='trade_code',all='TRUE')
        CT<-CT[is.na(CT[,'trade_code'])!=1,]
        CT[is.na(CT)]<-0
        CT<-merge(CT,stock.open,by='trade_code') 
        CT<-merge(CT,stock.close,by='trade_code')
        CT<-merge(CT,stock.low,by='trade_code')
        CT<-unique(CT)
        
        stockcap<-sum(CT[,'holdold']*CT[,'close'],na.rm=TRUE)*100
        IFcap<-hedge[d-1,'hold']*futures2[,'close']*300 
        IFprofit<-IFcap-hedge[d-1,'IFcap'] 
        
        cash<-cash+IFprofit        
        cap<-stockcap+cash
        
        caplist[d,'datetime']<-period.a[d-1]
        caplist[d,'cap']<-cap
        caplist[d,'lowestcap']<-sum(CT[,'holdold']*CT[,'low'],na.rm=TRUE)*100+cash
        caplist[d,'return']<-cap/caplist[d-1,'cap']-1
        caplist[d,'return2']<-cap/caplist[caplist$datetime==max(trad[trad<period.a[d]]),'cap']-1
        caplist[d,'cash']<-cash
        caplist[d,'stockcap']<-stockcap
        caplist[d,'IFcap']<-IFcap
        
        ##########################  以上回报昨日收盘，以下调仓结果
        
        CT[,'holdnew']<-floor((cap*Invr*CT[,'weight'])/CT[,'close']/100)
        CT[,'change']<-CT[,'holdnew']-CT[,'holdold']
        CT[,'fee']<-abs(CT[,'change']*CT[,'open'])*100*fee.ratio1
        CT[which(CT$fee<5),'fee']<-0
        CT[which(CT$fee<5),'holdnew']<-CT[which(CT$fee<5),'holdold']
        CT[which(CT$fee<5),'change']<-0
        CT[CT$change<0,'tax']<-abs(CT[CT$change<0,'change'])*CT[CT$change<0,'open']*100*tax.ratio
        CT[,'impact']<-abs(CT[,'change']*CT[,'open'])*100*impact.ratio1
        CT<-unique(CT)
        
        cost<-sum(CT[,c('fee','tax','impact')],na.rm=TRUE)
        cash<-cash+sum(CT[,'change']*CT[,'open']*100,na.rm=TRUE)*(-1)-cost
        
        unchange<-hold.old[is.element(hold.old$trade_code,CT[is.na(CT$open)==1,'trade_code']),]
        newhold<-CT[is.na(CT$open)!=1,]
        newhold<-newhold[newhold[,'holdnew']>0,c('trade_code','holdnew')]
        colnames(newhold)<-c('trade_code','holdold')
        
        hold.old<-rbind(unchange,newhold)
        
        stockcost<-stockcost+cost
        #####
        
        
        hedge[d,'hold']<-(-1)*floor(stockcap*beta/300/futures2[,'close'])
        hedge[d,'IFcap']<-hedge[d,'hold']*futures[,'open']*300
        hedge[d,'cost']<-abs(hedge[d,'hold']-hedge[d-1,'hold'])*futures[ ,'open']*300*(fee.ratio2+impact.ratio2)  
        # IFcash<-IFcash-hedge[d,'cost']-(hedge[d,'hold']-hedge[d-1,'hold'])*futures[ ,'open']*300
        IFprofit<-hedge[d-1,'hold']*(futures[ ,'open']-futures2[,'close'])*300
        
        cash<-cash-hedge[d,'cost']+IFprofit
        
        
      }else {
        CT<-NULL
        temp<-merge(hold.old,stock.adj,by='trade_code')
        temp<-merge(temp,adjbase,by='trade_code')
        colnames(temp)<-c('trade_code','holdold','adjnew','adjbase')
        temp[,'holdold2']<-floor(temp[,'holdold']*temp[,'adjnew']/temp[,'adjbase'])
        adjbase<-stock.adj
        hold.old<-temp[,c('trade_code','holdold2')]
        colnames(hold.old)<-c('trade_code','holdold')
        CT<-merge(hold.old,stock.close,by='trade_code')
        CT<-merge(CT,stock.low,by='trade_code')
        CT<-unique(CT)
        
        IFcap<-hedge[d-1,'hold']*futures2[,'close']*300  
        IFprofit<-IFcap-hedge[d-1,'IFcap']
        
        cash<-cash+IFprofit
        
        caplist[d,'datetime']<-period.a[d-1]
        stockcap<-sum(CT[,'holdold']*CT[,'close'],na.rm=TRUE)*100
        cap<-stockcap+cash
        
        caplist[d,'cap']<-cap
        caplist[d,'lowestcap']<-sum(CT[,'holdold']*CT[,'low'],na.rm=TRUE)*100+cash
        caplist[d,'return']<-cap/caplist[d-1,'cap']-1
        caplist[d,'cash']<-cash
        caplist[d,'stockcap']<-stockcap
        caplist[d,'IFcap']<-IFcap
        
        hedge[d,'hold']<-hedge[d-1,'hold']
        hedge[d,'IFcap']<-hedge[d,'hold']*futures2[,'close']*300      
        hedge[d,'cost']<-0    
      }  
    }
    
    #######for period.a end
    
    hedge[d,'trade_code']<-"IF1005.CFE"
    tt<-d
    
    if ( length(period.b)>=1){
      
      for ( d in 1:length(period.b)){
        
        sql1<-paste("SELECT trade_code,open from dailyprice WHERE datetime ='",period.b[d],"'")
        stock.open<-sqlQuery(ch,sql1)
        sql2<-paste("SELECT trade_code,close from dailyprice as a right join (SELECT max(datetime) as da from dailyprice where datetime<'",period.b[d],"') as b on a.datetime=b.da")
        stock.close<-sqlQuery(ch,sql2)
        sql3<-paste("SELECT trade_code,adjfactor from dailyprice as a right join (SELECT max(datetime) as da from dailyprice where datetime<'",period.b[d],"') as b on a.datetime=b.da")
        stock.adj<-sqlQuery(ch,sql3)
        sql4<-paste("SELECT trade_code,low from dailyprice as a right join (SELECT max(datetime) as da from dailyprice where datetime<'",period.b[d],"') as b on a.datetime=b.da")
        stock.low<-sqlQuery(ch,sql4)
        
        sqlbase<-paste("select datetime,trade_code, open,oi from hs300if where datetime='",period.b[d],"'")
        futures<-sqlQuery(ch,sqlbase)
        sqlbase2<-paste("SELECT datetime,trade_code,close from hs300if as a right join (SELECT max(datetime) as da from dailyprice where datetime<'",period.b[d],"') as b on a.datetime=b.da")
        futures2<-sqlQuery(ch,sqlbase2)
        
        
        hsif<-futures[which(futures[,'oi']==max(futures[,'oi'])),'trade_code']
        futures[,'trade_code']<-as.character(futures[,'trade_code'])
        futures2[,'trade_code']<-as.character(futures2[,'trade_code'])
        hedge[d+tt,'trade_code']<-as.character(hsif)
        
        
        if (is.element(period.b[d],trad)==1){   
          
          hold.new<-stock[[adjday[which(trad==period.b[d])]]]
          sql4<-paste("SELECT trade_code,amt from winddata.dailyprice WHERE datetime ='",period.b[d],"'")
          stock.amt<-sqlQuery(ch,sql4)
          hold.new<-merge(hold.new,stock.amt,by='trade_code')  
          add<-hold.new[is.na(hold.new[,'amt']),]
          add<-add[which(add[,'amt']>0),]
          addw<-0
          if (nrow(add)>0){
            addw<-sum(add[,'weight'])/(nrow(hold.new)-nrow(add))
          }
          hold.new[,'weight']<-hold.new[,'weight']+addw
          hold.new<-hold.new[is.na(hold.new[,'amt'])!=1,]
          hold.new<-hold.new[which(hold.new[,'amt']>0),]
          
          #分配资金用昨日收盘，
          #包含股票价格，股票代码的data.frame
          temp<-merge(hold.old,stock.adj,by='trade_code',all.x=TRUE)
          temp<-merge(temp,adjbase,by='trade_code')
          colnames(temp)<-c('trade_code','holdold','adjnew','adjbase')
          temp[,'holdold2']<-floor(temp[,'holdold']*temp[,'adjnew']/temp[,'adjbase'])
          adjbase<-stock.adj
          hold.old<-temp[,c('trade_code','holdold2')]
          colnames(hold.old)<-c('trade_code','holdold')
          
          CT<-NULL
          CT<-merge(hold.old,hold.new,by='trade_code',all='TRUE')
          CT<-CT[is.na(CT[,'trade_code'])!=1,]
          CT[is.na(CT)]<-0
          CT<-merge(CT,stock.open,by='trade_code') 
          CT<-merge(CT,stock.close,by='trade_code')
          CT<-merge(CT,stock.low,by='trade_code')
          CT<-unique(CT)
          
          stockcap<-sum(CT[,'holdold']*CT[,'close'],na.rm=TRUE)*100
          IFcap<-hedge[d+tt-1,'hold']*futures2[futures2[,'trade_code']==hedge[d+tt-1,'trade_code'],'close']*300
          IFprofit<-(IFcap-hedge[d+tt-1,'IFcap'])
          
          cash<-cash+IFprofit          
          cap<-stockcap+cash
          
          caplist[d+tt,'datetime']<-period[d+tt-1]
          caplist[d+tt,'cap']<-cap
          caplist[d+tt,'lowestcap']<-sum(CT[,'holdold']*CT[,'low'],na.rm=TRUE)*100+cash
          caplist[d+tt,'return']<-cap/caplist[d+tt-1,'cap']-1
          caplist[d+tt,'return2']<-cap/caplist[caplist$datetime==max(trad[trad<period.b[d]]),'cap']-1
          caplist[d+tt,'cash']<-cash
          caplist[d+tt,'stockcap']<-stockcap
          caplist[d+tt,'IFcap']<-IFcap
          
          if (nrow(CT)>0){
            CT[,'holdnew']<-floor((cap*Invr*CT[,'weight'])/CT[,'close']/100)
            CT[,'change']<-CT[,'holdnew']-CT[,'holdold']
            CT[,'fee']<-abs(CT[,'change']*CT[,'open'])*100*fee.ratio1
            CT[which(CT$fee<5),'fee']<-0
            CT[which(CT$fee<5),'holdnew']<-CT[which(CT$fee<5),'holdold']
            CT[which(CT$fee<5),'change']<-0
            CT[CT$change<0,'tax']<-abs(CT[CT$change<0,'change'])*CT[CT$change<0,'open']*100*tax.ratio
            CT[,'impact']<-abs(CT[,'change']*CT[,'open'])*100*impact.ratio1
            CT<-unique(CT)
            
            cost<-sum(CT[,c('fee','tax','impact')],na.rm=TRUE)
            cash<-cash+sum(CT[,'change']*CT[,'open']*100,na.rm=TRUE)*(-1)-cost
            
            unchange<-hold.old[is.element(hold.old$trade_code,CT[is.na(CT$open)==1,'trade_code']),]
            newhold<-CT[is.na(CT$open)!=1,]
            newhold<-newhold[newhold[,'holdnew']>0,c('trade_code','holdnew')]
            colnames(newhold)<-c('trade_code','holdold')
            
            hold.old<-rbind(unchange,newhold)
            
            stockcost<-stockcost+cost
            
          }else{
            hold.old<-hold.new
            colnames(hold.old)<-c('trade_code','holdold') 
            cost<-0
          }
          
          #####
          
          if (hsif==hedge[d+tt-1,'trade_code']){
            
            hedge[d+tt,'hold']<-(-1)*floor(stockcap*beta/300/futures2[futures2[,'trade_code']==hsif,'close'])
            hedge[d+tt,'cost']<-abs(hedge[d+tt,'hold']-hedge[d+tt-1,'hold'])*futures[futures[,'trade_code']==hsif,'open']*300*(fee.ratio2+impact.ratio2)       
            hedge[d+tt,'IFcap']<-hedge[d+tt,'hold']*futures[futures[,'trade_code']==hsif,'open']*300
            
            IFprofit<-hedge[d+tt-1,'hold']*(futures[futures[,'trade_code']==hsif,'open'])*300-IFcap
            cash<-cash+IFprofit
            cash<-cash-hedge[d+tt,'cost']
            
          }else {
            hedge[d+tt,'hold']<-(-1)*floor(stockcap*beta/300/futures2[futures2[,'trade_code']==hsif,'close'])
            IFprofit<-hedge[d+tt-1,'hold']*futures[futures[,'trade_code']==hedge[d+tt-1,'trade_code'],'open']*300-IFcap
            cash<-cash+IFprofit
            
            IFfee<-abs(hedge[d+tt,'hold'])*futures[futures[,'trade_code']==hsif,'open']*300*fee.ratio2
            IFfee<-IFfee+abs(hedge[d+tt-1,'hold'])*futures[futures[,'trade_code']==hedge[d+tt-1,'trade_code'],'open']*300*fee.ratio2
            IFimpact<-abs(hedge[d+tt,'hold'])*futures[futures[,'trade_code']==hsif,'open']*300*impact.ratio2
            IFimpact<-IFimpact+abs(hedge[d+tt-1,'hold'])*futures[futures[,'trade_code']==hedge[d+tt-1,'trade_code'],'open']*300*impact.ratio2
            
            hedge[d+tt,'cost']<- IFfee+IFimpact 
            hedge[d+tt,'IFcap']<-hedge[d+tt,'hold']*futures[futures[,'trade_code']==hsif,'open']*300
            
            cash<-cash-hedge[d+tt,'cost']
          } 
          
        }else {
          CT<-NULL
          temp<-merge(hold.old,stock.adj,by='trade_code',all.x=TRUE)
          temp<-merge(temp,adjbase,by='trade_code')
          colnames(temp)<-c('trade_code','holdold','adjnew','adjbase')
          temp[,'holdold2']<-floor(temp[,'holdold']*temp[,'adjnew']/temp[,'adjbase'])
          adjbase<-stock.adj
          hold.old<-temp[,c('trade_code','holdold2')]
          colnames(hold.old)<-c('trade_code','holdold')
          CT<-merge(hold.old,stock.close,by='trade_code')
          CT<-merge(CT,stock.low,by='trade_code')
          CT<-unique(CT)
          
          stockcap<-sum(CT[,'holdold']*CT[,'close'],na.rm=TRUE)*100
          IFcap<-hedge[d+tt-1,'hold']*futures2[futures2[,'trade_code']==hedge[d+tt-1,'trade_code'],'close']*300
          IFprofit<-IFcap-hedge[d+tt-1,'IFcap']
          
          cash<-cash+IFprofit          
          cap<-stockcap+cash
          
          caplist[d+tt,'datetime']<-period[d+tt-1]    
          caplist[d+tt,'cap']<-cap
          caplist[d+tt,'lowestcap']<-sum(CT[,'holdold']*CT[,'low'],na.rm=TRUE)*100+cash
          caplist[d+tt,'return']<-cap/caplist[d+tt-1,'cap']-1
          caplist[d+tt,'cash']<-cash
          caplist[d+tt,'stockcap']<-stockcap
          caplist[d+tt,'IFcap']<-IFcap
          
          if (hsif==hedge[d+tt-1,'trade_code']){
            hedge[d+tt,'hold']<-hedge[d+tt-1,'hold']
            IFcap<-hedge[d+tt-1,'hold']*futures2[futures2[,'trade_code']==hsif,'close']*300
            hedge[d+tt,'IFcap']<-IFcap
            hedge[d+tt,'cost']<-0    
          }else {
            
            hedge[d+tt,'hold']<-(-1)*floor(stockcap*beta/300/futures[futures[,'trade_code']==hsif,'open'])
            
            IFprofit<-hedge[d+tt-1,'hold']*futures[futures[,'trade_code']==hedge[d+tt-1,'trade_code'],'open']*300-IFcap
            cash<-cash+IFprofit #平仓利润
            
            IFfee<-abs(hedge[d+tt,'hold'])*futures[futures[,'trade_code']==hsif,'open']*300*fee.ratio2
            IFfee<-IFfee+abs(hedge[d+tt-1,'hold'])*futures[futures[,'trade_code']==hedge[d+tt-1,'trade_code'],'open']*300*fee.ratio2        
            IFimpact<-abs(hedge[d+tt,'hold'])*futures[futures[,'trade_code']==hsif,'open']*300*impact.ratio2
            IFimpact<-IFimpact+abs(hedge[d+tt-1,'hold'])*futures[futures[,'trade_code']==hedge[d+tt-1,'trade_code'],'open']*300*impact.ratio2
            
            hedge[d+tt,'cost']<- IFfee+IFimpact
            hedge[d+tt,'IFcap']<-hedge[d+tt,'hold']*futures[futures[,'trade_code']==hsif,'open']*300
            cash<-cash-IFfee-IFimpact
          } 
        } 
      }   
    }
    ##########if period.b>0 end
    ##########以下为只有period.b部分
  }else{
    
    sqlbase<-paste("select trade_code,open from index1 where datetime='",period.b[1],"'")
    futures<-sqlQuery(ch,sqlbase)
    sqlbase2<-paste("SELECT trade_code,close from index1 as a right join (SELECT max(datetime) as da from index1 where datetime<'",period.b[1],"') as b on a.datetime=b.da")
    futures2<-sqlQuery(ch,sqlbase2)
    sql3<-paste("SELECT trade_code,adjfactor from dailyprice where datetime='",period.b[1],"'")
    stock.adj<-sqlQuery(ch,sql3)
    
    hedge<-data.frame()
    
    hedge[1,'trade_code']<-as.character(futures[,'trade_code'])
    hedge[1,'hold']<-(-1)*floor(stockcap*beta/300/futures[,'open'])
    
    IFfee<-abs(hedge[1,'hold'])*fee.ratio2*futures[ ,'open']*300
    IFimpact<-abs(hedge[1,'hold'])*impact.ratio2*futures[ ,'open']*300
    
    hedge[1,'cost']<-IFfee+IFimpact
    hedge[1,'IFcap']<-hedge[1,'hold']*futures[ ,'open']*300
    
    # IFcash<-(-1)*hedge[1,'hold']*futures[ ,'open']*300-hedge[,'cost'] 
    adjbase<-stock.adj
    cash<-cash-IFfee-IFimpact
    
    #######
    for ( d in 2:length(period.b)){
      
      sql1<-paste("SELECT trade_code,open from winddata.dailyprice WHERE datetime ='",period.b[d],"'")
      stock.open<-sqlQuery(ch,sql1)
      sql2<-paste("SELECT trade_code,close from dailyprice where datetime='",period.b[d-1],"'")
      stock.close<-sqlQuery(ch,sql2)
      sql3<-paste("SELECT trade_code,adjfactor from dailyprice where datetime='",period.b[d-1],"'")
      stock.adj<-sqlQuery(ch,sql3)
      sql4<-paste("SELECT trade_code,low from dailyprice where datetime='",period.b[d-1],"'")
      stock.low<-sqlQuery(ch,sql4)
      
      sqlbase<-paste("select datetime,trade_code, open from index1 where datetime='",period.b[d],"'")
      futures<-sqlQuery(ch,sqlbase)
      sqlbase2<-paste("SELECT datetime,trade_code, close from index1 where datetime='",period.b[d-1],"'")
      futures2<-sqlQuery(ch,sqlbase2)
      
      
      hedge[d,'trade_code']<-as.character(futures[,'trade_code'])
      
      
      if (is.element(period.b[d],trad)==1){   
        
        hold.new<-stock[[adjday[which(trad==period.b[d])]]]
        sql4<-paste("SELECT trade_code,amt from winddata.dailyprice WHERE datetime ='",period.b[d],"'")
        stock.amt<-sqlQuery(ch,sql4)
        hold.new<-merge(hold.new,stock.amt,by='trade_code')  
        add<-hold.new[is.na(hold.new[,'amt']),]
        add<-add[which(add[,'amt']>0),]
        addw<-0
        if (nrow(add)>0){
          addw<-sum(add[,'weight'])/(nrow(hold.new)-nrow(add))
        }
        hold.new[,'weight']<-hold.new[,'weight']+addw
        hold.new<-hold.new[is.na(hold.new[,'amt'])!=1,]
        hold.new<-hold.new[which(hold.new[,'amt']>0),]        
        #分配资金用昨日收盘，
        #包含股票价格，股票代码的data.frame
        temp<-merge(hold.old,stock.adj,by='trade_code',all.x=TRUE)
        temp<-merge(temp,adjbase,by='trade_code')
        colnames(temp)<-c('trade_code','holdold','adjnew','adjbase')
        temp[,'holdold2']<-floor(temp[,'holdold']*temp[,'adjnew']/temp[,'adjbase'])
        adjbase<-stock.adj
        hold.old<-temp[,c('trade_code','holdold2')]
        colnames(hold.old)<-c('trade_code','holdold')
        
        CT<-NULL
        CT<-merge(hold.old,hold.new,by='trade_code',all='TRUE')
        CT<-CT[is.na(CT[,'trade_code'])!=1,]
        CT[is.na(CT)]<-0
        CT<-merge(CT,stock.open,by='trade_code') 
        CT<-merge(CT,stock.close,by='trade_code')
        CT<-merge(CT,stock.low,by='trade_code')
        CT<-unique(CT)
        
        stockcap<-sum(CT[,'holdold']*CT[,'close'],na.rm=TRUE)*100
        IFcap<-hedge[d-1,'hold']*futures2[,'close']*300 
        IFprofit<-IFcap-hedge[d-1,'IFcap'] 
        
        cash<-cash+IFprofit        
        cap<-stockcap+cash
        
        caplist[d,'datetime']<-period.b[d-1]
        caplist[d,'cap']<-cap
        caplist[d,'lowestcap']<-sum(CT[,'holdold']*CT[,'low'],na.rm=TRUE)*100+cash
        caplist[d,'return']<-cap/caplist[d-1,'cap']-1
        caplist[d,'return2']<-cap/caplist[caplist$datetime==max(trad[trad<period.b[d]]),'cap']-1
        caplist[d,'cash']<-cash
        caplist[d,'stockcap']<-stockcap
        caplist[d,'IFcap']<-IFcap
        
        ##########################  以上回报昨日收盘，以下调仓结果
        
        CT[,'holdnew']<-floor((cap*Invr*CT[,'weight'])/CT[,'close']/100)
        CT[,'change']<-CT[,'holdnew']-CT[,'holdold']
        CT[,'fee']<-abs(CT[,'change']*CT[,'open'])*100*fee.ratio1
        CT[which(CT$fee<5),'fee']<-0
        CT[which(CT$fee<5),'holdnew']<-CT[which(CT$fee<5),'holdold']
        CT[which(CT$fee<5),'change']<-0
        CT[CT$change<0,'tax']<-abs(CT[CT$change<0,'change'])*CT[CT$change<0,'open']*100*tax.ratio
        CT[,'impact']<-abs(CT[,'change']*CT[,'open'])*100*impact.ratio1
        CT<-unique(CT)
        
        cost<-sum(CT[,c('fee','tax','impact')],na.rm=TRUE)
        cash<-cash+sum(CT[,'change']*CT[,'open']*100,na.rm=TRUE)*(-1)-cost
        
        unchange<-hold.old[is.element(hold.old$trade_code,CT[is.na(CT$open)==1,'trade_code']),]
        newhold<-CT[is.na(CT$open)!=1,]
        newhold<-newhold[newhold[,'holdnew']>0,c('trade_code','holdnew')]
        colnames(newhold)<-c('trade_code','holdold')
        
        hold.old<-rbind(unchange,newhold)
        
        stockcost<-stockcost+cost
        #####
        
        
        hedge[d,'hold']<-(-1)*floor(stockcap*beta/300/futures2[,'close'])
        hedge[d,'IFcap']<-hedge[d,'hold']*futures[,'open']*300
        hedge[d,'cost']<-abs(hedge[d,'hold']-hedge[d-1,'hold'])*futures[ ,'open']*300*(fee.ratio2+impact.ratio2)  
        # IFcash<-IFcash-hedge[d,'cost']-(hedge[d,'hold']-hedge[d-1,'hold'])*futures[ ,'open']*300
        IFprofit<-hedge[d-1,'hold']*(futures[ ,'open']-futures2[,'close'])*300
        
        cash<-cash-hedge[d,'cost']+IFprofit
        
        
      }else {
        CT<-NULL
        temp<-merge(hold.old,stock.adj,by='trade_code')
        temp<-merge(temp,adjbase,by='trade_code')
        colnames(temp)<-c('trade_code','holdold','adjnew','adjbase')
        temp[,'holdold2']<-floor(temp[,'holdold']*temp[,'adjnew']/temp[,'adjbase'])
        adjbase<-stock.adj
        hold.old<-temp[,c('trade_code','holdold2')]
        colnames(hold.old)<-c('trade_code','holdold')
        CT<-merge(hold.old,stock.close,by='trade_code')
        CT<-merge(CT,stock.low,by='trade_code')
        CT<-unique(CT)
        
        IFcap<-hedge[d-1,'hold']*futures2[,'close']*300  
        IFprofit<-IFcap-hedge[d-1,'IFcap']
        
        cash<-cash+IFprofit
        
        caplist[d,'datetime']<-period.b[d-1]
        stockcap<-sum(CT[,'holdold']*CT[,'close'],na.rm=TRUE)*100
        cap<-stockcap+cash
        
        caplist[d,'cap']<-cap
        caplist[d,'lowestcap']<-sum(CT[,'holdold']*CT[,'low'],na.rm=TRUE)*100+cash
        caplist[d,'return']<-cap/caplist[d-1,'cap']-1
        caplist[d,'cash']<-cash
        caplist[d,'stockcap']<-stockcap
        caplist[d,'IFcap']<-IFcap
        
        hedge[d,'hold']<-hedge[d-1,'hold']
        hedge[d,'IFcap']<-hedge[d,'hold']*futures2[,'close']*300      
        hedge[d,'cost']<-0    
      }  
    }    
  }
  ####################
  odbcClose(ch)
  return( caplist )
  
}


