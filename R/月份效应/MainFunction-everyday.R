#Copyright by zdqh_invest;
#Author:wang gaobin<313085945@qq.com>
#File description:this file return caplist everyday for position management
#ps:this file must run after the mysql database updated (nearly 5:00pm everyday )

##说明：此文件夹只需要每日更新数据库后，全部选定运行即可
rm(list=ls(all=TRUE))
#setwd("G:\\R program&data\\final") # in Author's computer
setwd("Z:\\team4share\\实盘策略\\月份效应")  # in team's computer
source("Part1_DrawData.R",encoding="utf-8")
source("Part2_PortfolioConstruct.R",encoding="utf-8")
source("Part3_Backtesthedge4.R",encoding="utf-8")
source("Part4_PlotCal2.R",encoding="utf-8")
source("Part5_IntoWind.R",encoding="utf-8")
source('dyna-hedge.R')

library(RODBC)
ch=odbcConnect('winddata',uid='root',pwd='123')

#drawdata
industry=read.csv("allAindustry.csv")
mylist=DrawData(starttime=as.Date("2004-12-01"),lengthn=128,industry)
save(mylist,file="mylist.RData")
#next month,the period=period+1

#portfolio construct
select_industry=read.table(file="output12.csv",header=T,sep=",") ##过些年份后，可能还要更新频数的统计表格；
stock_pick=PortfolioConstruct(mylist,select_industry,start_time="2005-01-01",period=127,yuzhi=6,bili=0.5,pick="hs300weight")
#next month,the period=period+1

#for position management
lastday=as.character(Sys.Date())
caplist_hs300_month_amt_0930=Backtesthedge4(30000000,0.85,0.9,stock_pick,0.00035,0.00003,0.001,0.002,0.00017,lastday,0.05,"hs300if0930")
caplist_hs300_month_amt_0915=Backtesthedge4(30000000,0.85,0.9,stock_pick,0.00035,0.00003,0.001,0.002,0.00017,lastday,0.05,"hs300if0915")
caplist1=caplist_hs300_month_amt_0930[[1]]
output_300<-dyna_hedge(caplist1,stock_pick,1,0.003,lastday,"hs300if0930")
write.csv(output_300,file=paste("Z:\\team4share\\实盘策略\\回测\\test\\MonthlyEffect300_dyna_hed_0930","-",Sys.Date(),".csv",sep=""),row.names=F)
write.csv(output_300,file=paste("Z:\\team4share\\实盘策略\\仓位管理\\MonthlyEffect300_dyna_hed_0930.csv",sep=""),row.names=F)

caplist2=caplist_hs300_month_amt_0915[[1]]
name1="MonthlyEffect300_0930.csv"
name2="MonthlyEffect300_0915.csv"
setwd("Z:\\实盘策略\\仓位管理")
write.csv(caplist1,file=name1,row.names=F)
write.csv(caplist2,file=name2,row.names=F)

##file backup
name1=paste("caplist_month_effect","_","hs300if0930","-",lastday,sep="")
name3=paste(name1,".csv",sep="")
#setwd("G:\\R program&data\\final\\backtest")
setwd("Z:\\实盘策略\\月份效应\\backtest")
future=paste(name1,".jpg")
jpeg(file=future)
PlotCal2(caplist1[,2]/caplist1[1,2],caplist1[,1],t=name1)
dev.off()
write.csv(caplist1,file=name3,row.names=F)

name1=paste("caplist_month_effect","_","hs300if0915","-",lastday,sep="")
name3=paste(name1,".csv",sep="")
#setwd("G:\\R program&data\\final\\backtest")
setwd("Z:\\实盘策略\\月份效应\\backtest")
future=paste(name1,".jpg")
jpeg(file=future)
PlotCal2(caplist2[,2]/caplist2[1,2],caplist2[,1],t=name1)
dev.off()
write.csv(caplist2,file=name3,row.names=F)

odbcClose(ch)