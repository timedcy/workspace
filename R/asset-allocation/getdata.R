library("WindR")
w.start(showmenu=F)
#获取hs300指数,每月末数据
hs300<-w.wsd("000300.SH","close,pct_chg","2002-01-01","2015-09-30",
                         "period=3;returnType=1;Period=M;Fill=Previous;PriceAdj=F")
hs300<-data.frame(hs300[2])
#获取hs300子行业指数
hs300_sub<-w.wsd("000908.SH,000909.SH,000910.SH,000911.SH,000912.SH,000913.SH,000914.SH,000915.SH,000916.SH,000917.SH","close",
                 "2002-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=F")
hs300_sub<-data.frame(hs300_sub[2])

#获取上证国债、10年期国债
#szbond<-w.wsd("000012.SH","close,pct_chg","2003-01-01","2015-09-30","Fill=Previous;PriceAdj=DP")
#szbond<-data.frame(szbond[2])

#bond10<-w.wsd("H11077.SH","close,pct_chg","2003-01-01","2015-09-30","Fill=Previous;PriceAdj=DP")
#bond10<-data.frame(bond10[2])

#获取中证全债、50债、3债、7债、10债、10+债全价
zzbond<-w.wsd("H11001.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=DP")
zzbond50<-w.wsd("H11016.CSI","close,pct_chg","2008-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=DP")
zzbond3<-w.wsd("H11002.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=DP")
zzbond7<-w.wsd("H11003.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=DP")
zzbond10<-w.wsd("H11004.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=DP")
zzbond20<-w.wsd("H11005.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=DP")

zzbond<-data.frame(zzbond[2])
zzbond50<-data.frame(zzbond50[2])
zzbondsub<-data.frame(zzbond3[2][[1]][1:2],zzbond7[2][[1]][,2],zzbond10[2][[1]][,2],zzbond20[2][[1]][,2])

#获取中证全债、50债、3债、7债、10债、10+债净价
net_zzbond<-w.wsd("H01001.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=CP")
net_zzbond50<-w.wsd("H01016.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=CP")
net_zzbond3<-w.wsd("H01002.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=CP")
net_zzbond7<-w.wsd("H01003.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=CP")
net_zzbond10<-w.wsd("H01004.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=CP")
net_zzbond20<-w.wsd("H01005.CSI","close,pct_chg","2003-01-01","2015-09-30","Period=M;Fill=Previous;PriceAdj=CP")

net_zzbond<-data.frame(net_zzbond[2])
net_zzbond50<-data.frame(net_zzbond50[2])
net_zzbondsub<-data.frame(net_zzbond3[2][[1]][1:2],net_zzbond7[2][[1]][,2],net_zzbond10[2][[1]][,2],net_zzbond20[2][[1]][,2])

#获取中证商品期货综合指数和农产品、金属、化工、能源综合指数
zzcf<-w.wsd("H11061.CSI","close,pct_chg","2005-01-01","2015-09-30","Period=M;Fill=Previous")
zzaf<-w.wsd("H11062.CSI","close,pct_chg","2005-01-01","2015-09-30","Period=M;Fill=Previous")
zzmf<-w.wsd("H11063.CSI","close,pct_chg","2005-01-01","2015-09-30","Period=M;Fill=Previous")
zzcpf<-w.wsd("H11064.CSI","close,pct_chg","2005-01-01","2015-09-30","Period=M;Fill=Previous")
zzef<-w.wsd("H11065.CSI","close,pct_chg","2005-01-01","2015-09-30","Period=M;Fill=Previous")

zzcf<-data.frame(zzcf$Data)
zzcf_sub<-data.frame(zzaf[2][[1]],zzmf[2][[1]][,2],zzcpf[2][[1]][,2],zzef[2][[1]][,2])