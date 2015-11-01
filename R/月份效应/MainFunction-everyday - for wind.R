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
library(RODBC)
ch=odbcConnect('winddata',uid='root',pwd='123')

#drawdata
industry=read.csv("allAindustry.csv")
mylist=DrawData(starttime=as.Date("2004-12-01"),lengthn=129,industry)
save(mylist,file="mylist.RData")
#next month,the period=period+1

#portfolio construct
select_industry=read.table(file="output12.csv",header=T,sep=",") ##过些年份后，可能还要更新频数的统计表格；
stock_pick=PortfolioConstruct(mylist,select_industry,start_time="2005-01-01",period=128,yuzhi=6,bili=0.5,pick="hs300weight")
#next month,the period=period+1
beta=0.9
tpn=IntoWind(stock_pick,beta)
k<-tpn[,c(1,2,3,5)]

setwd("Z:\\team4share")

write.csv(k,file="month300.csv",row.names=F)


odbcClose(ch)
