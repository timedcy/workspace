setwd("Z:/zxt/code5")
source('Backtesthedge5.R')
source('Part4 plot.R')
source('signal.R')
source('dyna-hedge.R')
load("allA4-all-83.RData")
allA0.04stock<-stock_pick
load("allA6-all-83.RData")
allA0.06stock<-stock_pick

date<-Sys.Date();  #回测日期
rate<-0.05


##########0930
  #hs300
  #impact.ratio<-0.002; 
  zz500ic="zz500ic0930"
  #caplist1<-Backtesthedge4(30000000,0.80,0.9,hs300stock,0.00035,0.00003,0.001,impact.ratio,0.00017,date,rate,hs300if)[[1]]
#output_300<-dyna_hedge(caplist1,hs300stock,1,impact.ratio,date,hs300if,0.05)
#write.csv(output_300,file=paste("Z:\\实盘策略\\回测\\test\\cap30080_dyna_hed_0930","-",Sys.Date(),".csv",sep=""),row.names=F)
#write.csv(output_300,file=paste("Z:\\实盘策略\\仓位管理\\cap30080_dyna_hed_0930.csv",sep=""),row.names=F)

#plot_cal2(caplist1[,2]/caplist1[1,2],caplist1[,1])
#write.csv(caplist1,file=paste("Z:\\实盘策略\\仓位管理\\cap30080_0930.csv",sep=""),row.names=F)
#write.csv(caplist1,file=paste("Z:\\实盘策略\\回测\\每日回测备份\\cap30080_0930","-",Sys.Date(),".csv",sep=""),row.names=F)
  #allA0.04
  impact.ratio<-0.004;
  caplist2<-Backtesthedge5(30000000,0.85,0.9,allA0.04stock,0.00035,0.00003,0.001,impact.ratio,0.00017,date,rate,zz500ic)[[1]]
#output_all0.04<-dyna_hedge(caplist2,allA0.04stock,1,impact.ratio,date,zz500ic,0.05)
#write.csv(output_all0.04,file=paste("Z:\\实盘策略\\回测\\test\\capallA0.04_dyna_hed_0930","-",Sys.Date(),".csv",sep=""),row.names=F)
#write.csv(output_all0.04,file=paste("Z:\\实盘策略\\仓位管理\\capallA0.04_dyna_hed_0930.csv",sep=""),row.names=F)

plot_cal2(caplist2[,2]/caplist2[1,2],caplist2[,1])  
write.csv(caplist2,file=paste("Z:\\实盘策略\\仓位管理\\capallA0.04_0930.csv",sep=""),row.names=F)
write.csv(caplist2,file=paste("Z:\\实盘策略\\回测\\每日回测备份\\capallA0.04_0930","-",Sys.Date(),".csv",sep=""),row.names=F)

#allA0.06

caplist3<-Backtesthedge5(30000000,0.85,0.9,allA0.06stock,0.00035,0.00003,0.001,impact.ratio,0.00017,date,rate,zz500ic)[[1]]
#output_all0.06<-dyna_hedge(caplist3,allA0.06stock,1,impact.ratio,date,zz500ic,0.05)
#write.csv(output_all0.06,file=paste("Z:\\实盘策略\\回测\\test\\capallA0.06_dyna_hed_0930","-",Sys.Date(),".csv",sep=""),row.names=F)
#write.csv(output_all0.06,file=paste("Z:\\实盘策略\\仓位管理\\capallA0.06_dyna_hed_0930.csv",sep=""),row.names=F)

plot_cal2(caplist3,2]/caplist3[1,2],caplist3[,1])  
write.csv(caplist3,file=paste("Z:\\实盘策略\\仓位管理\\capallA0.06_0930.csv",sep=""),row.names=F)
write.csv(caplist3,file=paste("Z:\\实盘策略\\回测\\每日回测备份\\capallA0.06_0930","-",Sys.Date(),".csv",sep=""),row.names=F)

##########0915
#hs300
#impact.ratio<-0.002; 
#hs300if="hs300if0915"
#caplist3<-Backtesthedge4(30000000,0.80,0.9,hs300stock,0.00035,0.00003,0.001,impact.ratio,0.00017,date,rate,hs300if)[[1]]
#plot_cal2(caplist3[,2]/caplist3[1,2],caplist3[,1])
#write.csv(caplist3,file=paste("Z:\\实盘策略\\仓位管理\\cap30080_0915.csv",sep=""),row.names=F)
#write.csv(caplist3,file=paste("Z:\\实盘策略\\回测\\每日回测备份\\cap30080_0915","-",Sys.Date(),".csv",sep=""),row.names=F)
#zz800
#impact.ratio<-0.004;
#caplist4<-Backtesthedge4(30000000,0.80,0.9,zz800stock,0.00035,0.00003,0.001,impact.ratio,0.00017,date,rate,hs300if)[[1]]
#plot_cal2(caplist4[,2]/caplist4[1,2],caplist4[,1])
#write.csv(caplist4,file=paste("Z:\\实盘策略\\仓位管理\\cap8005_0915.csv",sep=""),row.names=F)
#write.csv(caplist4,file=paste("Z:\\实盘策略\\回测\\每日回测备份\\cap8005_0915","-",Sys.Date(),".csv",sep=""),row.names=F)


#for hs300 and month300
Signal_Dynamic_beat(0.05)

#for zz800
Signal_Dynamic_beat(0.09)



