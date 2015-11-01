source("getdata.R")
#行业配置处理函数
data<-function(stock,bond,commodity){
  #获取最短的序列长度
  n<-min(length(stock),length(bond),length(commodity))
  stock<-stock[(length(stock)-n+1):length(stock)]
  bond<-bond[(length(bond)-n+1):length(bond)]
  commodity<-commodity[(length(commodity)-n+1):length(commodity)]
  price<-data.frame(stock,bond,commodity)
  return(price) 
}

volability<-function(price){
  #根据收益序列得到波动率
  sigma<-matrix(0,(dim(price)[1]-12),dim(price)[2])
  for (i in 1:dim(price)[2]){
    ret<-diff(log(price[,i]))
    for (j in 1:(length(ret)-11)){
      sigma[j,i]<-sd(ret[j:(j+11)])*sqrt(12)
    }
  }
  return(sigma)
}

proportion<-function(sigma){
  #资产配置比例
  ratio<-matrix(0,dim(sigma)[1],dim(sigma)[2])
  for (i in 1:dim(sigma)[1]){
    for (j in 1:dim(sigma)[2]){
      if(j == 1){
        ratio[i,1]<-1/(1 + sum(sigma[i,1]/sigma[i,2:dim(sigma)[2]]))
      }else{
        ratio[i,j]<-ratio[i,1]*sigma[i,1]/sigma[i,j]
      }
    }
  }
  ratio
}

#MaxDDFun最大回撤 annualized.return年化收益 annualized.SharpeRatio年化夏普
RetDrawdown<-function(ret) {
  ret[is.na(ret)] <- 0
  ret.cum<-cumsum(ret)
  drawdown<-ret.cum - cummax(ret.cum)
  drawdown
}

MaxDDFun <- function(ret) min(RetDrawdown(ret))
annualized.return <- function(return) mean(return, na.rm = T) * 12
annualized.SharpeRatio <- function(return) mean(return, na.rm = T)/sd(return, na.rm = T) * sqrt(12)

matSMA <- function(sub){
  mat <- matrix(NA, nrow = (dim(sub)[1]-9), ncol = (dim(sub)[2]-1))
  for(i in 1:(dim(sub)[2]-1)){
    mat[,i] <- SMA(sub[,i+1],n = 10)[10:dim(sub)[1]]
  }
  mat
}



