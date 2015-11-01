#11111111111111111##################################################

library(RODBC)
ch<-odbcConnect('winddata',uid='root',pwd='123')
trade_day<-sqlQuery(ch,'select distinct datetime from dailyprice')[,1]
trade_day_chr<-as.character(trade_day)

get_ttm<-function(datetime,fct,table){
aa<-paste('select trade_code,avg(',fct,')*4 as ',fct,'_ttm',' from (select trade_code,',fct,',@num := if( @trade_code=trade_code,@num+1,1  ) as row_number,@trade_code := trade_code as dummy,stm_issuingdate from ',table,' where stm_issuingdate<\'',datetime,'\'order by trade_code,datetime desc)as x where x.row_number<5 group by trade_code',sep='')  
sqlQuery(ch,'set @num := 0, @trade_code := \'\';')
aaa<-sqlQuery(ch,aa)
aaa[,1]<-as.character(aaa[,1])
rownames(aaa)<-aaa[,1]
aaa<-aaa[complete.cases(aaa),]
aaa[,2]<-round(aaa[,2],4)
return(aaa) 
}
#aaa<-get_ttm('2014-10-25','current','seasondata')
#22222222222222########################################################
get_yoy<-function(datetime,fct,table){
  aa<-paste('select a.trade_code,(a.',fct,'/b.',fct,'-1)*100 as yoy_',fct,' from(select trade_code,',fct,' from(select trade_code,datetime,',fct,',@num := if( @trade_code=trade_code,@num+1,1  ) as row_number,@trade_code := trade_code as dummy, stm_issuingdate from ',table,' where stm_issuingdate<\'',datetime,'\'order by trade_code,datetime desc)as x where x.row_number=1)as a INNER JOIN (select trade_code,',fct,' from(select trade_code,datetime,',fct,',@num := if( @trade_code=trade_code,@num+1,1  ) as row_number,@trade_code := trade_code as dummy,stm_issuingdate from ',table,' where stm_issuingdate<\'',datetime,'\'order by trade_code,datetime desc )as x WHERE x.row_number=5)as b WHERE a.trade_code=b.trade_code',sep='')  
  sqlQuery(ch,'set @num := 0, @trade_code := \'\';')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa) 
}
#aaa<-get_yoy('2014-10-25','current','seasondata')
#33333333333#################################################
get_pe<-function(datetime,fct,fct_name,table){
  index<-which.max(trade_day-as.Date(datetime)>0)
  datetime<-trade_day_chr[index-1]
  aa<-paste('select aa.trade_code,close*total_shares/',fct,' as ',fct_name,' from(select a.trade_code,',fct,' from(select max(datetime)as datetime,trade_code from ',table,' where stm_issuingdate <\'',datetime,'\' GROUP BY trade_code) as a INNER JOIN ',table,' as b where a.datetime=b.datetime and a.trade_code=b.trade_code)as aa INNER JOIN (SELECT trade_code,close,total_shares from dailyprice where datetime=\'',datetime,'\')as bb where aa.trade_code=bb.trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa) 
}
#aaa<-get_pe('2014-10-24','tot_equity','pe','seasondata')
#44444444444444444#############################################################
get_avg<-function(datetime,period,fct){
  index<-which.max(trade_day-as.Date(datetime)>0)
  datetime<-trade_day_chr[index-1]
  begtime<-trade_day_chr[index-period]
  aa<-paste('select trade_code, avg(',fct,') AS avg',fct,'_',period,' from dailyprice where (datetime BETWEEN (\'',begtime,'\') and \'',datetime,'\')and trade_status=1 group by trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa)   
}
#aaa<-get_avg('2014-11-07','2014-10-08','turn')
#55555###########################################################################3
get_norm<-function(datetime,fct,table){
  aa<-paste('SELECT a.trade_code,',fct,' from(SELECT max(datetime)as datetime,trade_code from ',table,' WHERE stm_issuingdate <\'',datetime,'\' GROUP BY trade_code) as a INNER JOIN ',table,' as b where a.datetime=b.datetime and a.trade_code=b.trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa)
}
#aaa<-get_norm('2014-10-25','current','seasondata')
#66666666########################################################################
get_chg<-function(datetime,period){
  index<-which.max(trade_day-as.Date(datetime)>0)
  datetime<-trade_day_chr[index-1]
  begtime<-trade_day_chr[index-period]
  aa<-paste('select b.trade_code,(a.price0/b.price30-1) as pct_chg from(select trade_code,`close`*adjfactor AS price0  from dailyprice where datetime =\'',datetime,'\')as a INNER JOIN(select trade_code,`close`*adjfactor AS price30  from dailyprice where datetime =\'',begtime,'\') as b where a.trade_code=b.trade_code ORDER BY trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa)
}
#aaa<-get_chg('2014-11-07','2014-10-08')
#777777777777777###########################################################33
get_htol<-function(datetime,period){
  index<-which.max(trade_day-as.Date(datetime)>0)
  datetime<-trade_day_chr[index-1]
  begtime<-trade_day_chr[index-period]
  aa<-paste('select trade_code, max(close*adjfactor)/min(close*adjfactor) AS hightolow from dailyprice where (datetime BETWEEN (\'',begtime,'\') and \'',datetime,'\') group by trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa)      
}
#aaa<-get_htol('2014-11-07','2014-10-08')
#888888888888888888#########################################################
get_rsi<-function(datetime,period){
  index<-which.max(trade_day-as.Date(datetime)>0)
  datetime<-trade_day_chr[index-1]
  begtime<-trade_day_chr[index-period]
  aa<-paste('SELECT a.trade_code,100-100/(1+a.up/b.down) as rsi from(select trade_code,sum(pct_chg)as up from dailyprice where ((datetime BETWEEN \'',begtime,'\'and\'',datetime,'\')and pct_chg>0) GROUP BY trade_code)as a INNER JOIN(select trade_code,-sum(pct_chg)as down from dailyprice where ((datetime BETWEEN \'',begtime,'\'and\'',datetime,'\')and pct_chg<0)GROUP BY trade_code)as b WHERE a.trade_code=b.trade_code ORDER BY trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa)
}
#aaa<-get_rsi('2014-11-07','2014-10-08')
###################################################
get_atob<-function(datetime,a,b,table){
  aa<-paste('SELECT aa.trade_code,',a,'/',b,' as ',a,'to',b,' from(SELECT max(datetime)as datetime,trade_code from ',table,' WHERE stm_issuingdate <\'',datetime,'\' GROUP BY trade_code) as aa INNER JOIN ',table,' as bb where aa.datetime=bb.datetime and aa.trade_code=bb.trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa)  
}
#aaa<-get_atob('2014-10-25','roe_diluted','roa2','seasona')
get_price<-function(datetime,fct,table){
  index<-which.max(trade_day-as.Date(datetime)>=0)
  datetime<-trade_day_chr[index-1]
  aa<-paste('SELECT trade_code,',paste(fct,collapse = ','),' from ',table,' where datetime=\'',datetime,'\'',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2:ncol(aaa)]<-round(aaa[,2:ncol(aaa)],4)
  return(aaa)  
}

get_mton<-function(datetime,m,mtable,n,ntable,fct_name){
  aa<-paste('SELECT aaa.trade_code,',m,'/',n,' as ',fct_name,' FROM(SELECT aa.datetime,aa.trade_code,',m,' from(SELECT max(datetime)as datetime,trade_code from ',mtable,' WHERE stm_issuingdate <\'',datetime,'\' GROUP BY trade_code) as aa INNER JOIN ',mtable,' as bb where aa.datetime=bb.datetime and aa.trade_code=bb.trade_code)as aaa INNER JOIN ',ntable,' as bbb where aaa.datetime=bbb.datetime and aaa.trade_code=bbb.trade_code',sep='')
  aaa<-sqlQuery(ch,aa)
  aaa[,1]<-as.character(aaa[,1])
  rownames(aaa)<-aaa[,1]
  aaa<-aaa[complete.cases(aaa),]
  aaa[,2]<-round(aaa[,2],4)
  return(aaa)
}
















#library(RODBC)
#ch<-odbcConnect('winddata',uid='root',pwd='123')
#sqlQuery(ch,'set @num := 0, @trade_code := \'\';')
#aa<-paste('SELECT trade_code,datetime,stm_issuingdate,avg(qfa_tot_oper_rev)*4 as ttm_tot_oper_rev,avg(qfa_tot_oper_cost)*4 as ttm_tot_oper_cost,avg(qfa_grossmargin)*4 as ttm_grossmargin,avg(qfa_selling_dist_exp)*4 as ttm_selling_dist_exp,avg(qfa_gerl_admin_exp)*4 as ttm_gerl_admin_exp,avg(qfa_fin_exp_is)*4 as ttm_fin_exp_is,avg(qfa_opprofit)*4 as ttm_opprofit,avg(qfa_tot_profit)*4 as ttm_tot_profit,avg(qfa_net_profit_is)*4 as ttm_net_profit_is,avg(qfa_np_belongto_parcomsh)*4 as ttm_np_belongto_parcomsh,avg(qfa_tot_compreh_inc_parent_comp)*4 as ttm_tot_compreh_inc_parent_comp,avg(qfa_net_cash_flows_oper_act)*4 as ttm_net_cash_flows_oper_act,avg(qfa_net_cash_flows_inv_act)*4 as ttm_net_cash_flows_inv_act,avg(qfa_net_cash_flows_fnc_act) as ttm_net_cash_flows_fnc_act from(select trade_code,datetime,stm_issuingdate,qfa_tot_oper_rev,qfa_tot_oper_cost,qfa_grossmargin,qfa_selling_dist_exp,qfa_gerl_admin_exp,qfa_fin_exp_is,qfa_opprofit,qfa_tot_profit,qfa_net_profit_is,qfa_np_belongto_parcomsh,qfa_tot_compreh_inc_parent_comp,qfa_net_cash_flows_oper_act,qfa_net_cash_flows_inv_act,qfa_net_cash_flows_fnc_act FROM (select trade_code,datetime,qfa_tot_oper_rev,qfa_tot_oper_cost,qfa_grossmargin,qfa_selling_dist_exp,qfa_gerl_admin_exp,qfa_fin_exp_is,qfa_opprofit,qfa_tot_profit,qfa_net_profit_is,qfa_np_belongto_parcomsh,qfa_tot_compreh_inc_parent_comp,qfa_net_cash_flows_oper_act,qfa_net_cash_flows_inv_act,qfa_net_cash_flows_fnc_act,@num := if( @trade_code=trade_code,@num+1,1  ) as row_number,@trade_code := trade_code as dummy,stm_issuingdate from seasonqfa where datetime<=\'','2014-09-30','\' order by trade_code,datetime DESC)as x WHERE x.row_number<5) as y GROUP BY trade_code HAVING count(*)=4',sep='')
#aaa<-sqlQuery(ch,aa)
#sqlSave(ch,aaa,'seasonttm',rownames=F)













