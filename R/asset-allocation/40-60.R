setwd("C:/Users/liuli/Documents/workspace/asset-allocation")
source("getdata.R")
source("dataproc.R")
library("TTR")

#股票、债券、商品期货三个大类价格、波动率、配置比例
ratio <- c(0.6,0.4)

#HS300子行业、债券3、5、10、10+期限、商品期货细分
sigma_hs300sub<-volability(hs300_sub[2:11])
sigma_bondsub<-volability(net_zzbondsub[2:5])

ratio_hs300sub<-proportion(sigma_hs300sub)
ratio_bondsub<-proportion(sigma_bondsub)





#建仓从2006-01-25开始 49 37 13
capital_base<-100000
capital<-c()
for(i in 1:117){
  #每期开始大类资产配置资金
  if(i==1){
    capital_sub<-capital_base*ratio
  }else{
    capital_sub<-capital[(i-1)]*ratio
  }
  cap_hs300sub<-capital_sub[1]*ratio_hs300sub[i,]
  cap_bondsub<-capital_sub[2]*ratio_bondsub[i,]
  
  cap_hs300sub<-cap_hs300sub*(hs300_sub[(i+49),2:11]/hs300_sub[(i+48),2:11])
  cap_bondsub<-cap_bondsub*(zzbondsub[(i+37),2:5]/zzbondsub[(i+36),2:5])
  
  capital[i]<-sum(cap_hs300sub)+sum(cap_bondsub)
}

#收益率序列、最大回撤、年华收益、年华夏普比率
ret<-diff(log(capital))
maxDrawdown<-MaxDDFun(ret)
annuReturn<-annualized.return(ret)
annuShape<-annualized.SharpeRatio(ret)
plot(zzcf_sub[13:129,1],capital,type="l")








#10个月SMA作为建仓条件
sma_hs300sub <- matSMA(hs300_sub)
sma_zzbondsub <- matSMA(net_zzbondsub)
sma_zzcfsub <- matSMA(zzcf_sub)

#建仓从2006-01-25
capital_base<-100000
capital<-c()
#每日头寸
pos <- matrix(0,nrow = 117, ncol = 14)
all <- cbind(hs300_sub[(dim(hs300_sub)[1]-117+1):dim(hs300_sub)[1],2:11],
             net_zzbondsub[(dim(net_zzbondsub)[1]-117+1):dim(net_zzbondsub)[1],2:5])

sma <- cbind(sma_hs300sub[(dim(sma_hs300sub)[1]-117+1):dim(sma_hs300sub)[1],],
             sma_zzbondsub[(dim(sma_zzbondsub)[1]-117+1):dim(sma_zzbondsub)[1],])

for(i in 1:14){
  for(j in 1:117){
    if(all[j,i] > sma[j,i]){
      pos[j:117,i] <- 1
    }else{
      pos[j:117,i] <- 0
    }
  }
}

for(i in 1:117){
  
  #每期开始大类资产配置资金
  if(i==1){
    capital_sub<-capital_base*ratio
  }else{
    capital_sub<-capital[(i-1)]*ratio
  }
  cap_hs300sub<-capital_sub[1]*ratio_hs300sub[i,]
  cap_bondsub<-capital_sub[2]*ratio_bondsub[i,]
  
  for(j in 1:10){
    if(pos[i,j] == 1){
      cap_hs300sub[j]<-cap_hs300sub[j]*(hs300_sub[(i+49),(j+1)]/hs300_sub[(i+48),(j+1)])
    }
  }
  
  for(j in 11:14){
    if(pos[i,j] == 1){
      cap_bondsub[(j-10)]<-cap_bondsub[(j-10)]*(zzbondsub[(i+37),(j-9)]/zzbondsub[(i+36),(j-9)])
    }
  }
  
  capital[i]<-sum(cap_hs300sub)+sum(cap_bondsub)
}

#收益率序列、最大回撤、年华收益、年华夏普比率
ret<-diff(log(capital))
maxDrawdown<-MaxDDFun(ret)
annuReturn<-annualized.return(ret)
annuShape<-annualized.SharpeRatio(ret)
plot(zzcf_sub[13:129,1],capital,type="l")




#股票和债券不同比例下最后收益

x <- seq(0,1,0.01)
y <- 1- x
annuReturn <- c()
maxDrawdown <- c()
annuShape <- c()

for(h in 1:101){
  ratio <- c(x[h],y[h])
  for(i in 1:117){
    
    #每期开始大类资产配置资金
    if(i==1){
      capital_sub<-capital_base*ratio
    }else{
      capital_sub<-capital[(i-1)]*ratio
    }
    cap_hs300sub<-capital_sub[1]*ratio_hs300sub[i,]
    cap_bondsub<-capital_sub[2]*ratio_bondsub[i,]
    
    for(j in 1:10){
      if(pos[i,j] == 1){
        cap_hs300sub[j]<-cap_hs300sub[j]*(hs300_sub[(i+49),(j+1)]/hs300_sub[(i+48),(j+1)])
      }
    }
    
    for(j in 11:14){
      if(pos[i,j] == 1){
        cap_bondsub[(j-10)]<-cap_bondsub[(j-10)]*(zzbondsub[(i+37),(j-9)]/zzbondsub[(i+36),(j-9)])
      }
    }
    
    capital[i]<-sum(cap_hs300sub)+sum(cap_bondsub)
  }
  ret<-diff(log(capital))
  maxDrawdown[h]<-MaxDDFun(ret)
  annuReturn[h]<-annualized.return(ret)
  annuShape[h]<-annualized.SharpeRatio(ret)
   
}






