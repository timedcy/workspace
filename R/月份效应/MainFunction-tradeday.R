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
mylist=DrawData(starttime=as.Date("2004-12-01"),lengthn=130,industry)
save(mylist,file="mylist.RData")
#next month,the period=period+1
##127 4/30
#portfolio construct
select_industry=read.table(file="output12.csv",header=T,sep=",") ##过些年份后，可能还要更新频数的统计表格；
stock_pick=PortfolioConstruct(mylist,select_industry,start_time="2005-01-01",period=129,yuzhi=6,bili=0.5,pick="hs300weight")
stock_pick_backup=PortfolioConstruct(mylist,select_industry,start_time="2005-01-01",period=129,yuzhi=6,bili=0.6,pick="hs300weight")
#next month,the period=period+1
#126 4/30

setwd("Z:\\team4share\\实盘策略\\多因子选股模块\\数据集\\策略选股")
dir_name<-paste(Sys.Date(),"-",'month300',sep='')
path_name<-paste('./',dir_name,sep='')
dir.create(path_name)
path=paste("Z:\\team4share\\实盘策略\\多因子选股模块\\数据集\\策略选股","\\",dir_name,sep="")
setwd(path)
##this_stock_pick
#tradeday="2015-04-30"
tradeday=Sys.Date()
n=length(stock_pick)
this_stock_pick=stock_pick[[n]]
name1=paste(tradeday,"-","month300",".csv",sep="")
write.csv(this_stock_pick,file=name1,row.names=F)
##this_stock_pick_backup
n=length(stock_pick_backup)
this_stock_pick_backup=stock_pick_backup[[n]]
this_stock_pick_backup[which(is.element(this_stock_pick_backup[,1],this_stock_pick[,1])),"50%"]<-1
this_stock_pick_backup[which(!is.element(this_stock_pick_backup[,1],this_stock_pick[,1])),"50%"]<-0 
name2=paste(tradeday,"-","month300-backup",".csv",sep="")
write.csv(this_stock_pick_backup,file=name2,row.names=F)
