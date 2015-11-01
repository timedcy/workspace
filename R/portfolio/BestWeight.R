require(tseries)
require(DEoptim)
source("./Position Analysis.R")

#给定资产收益均值以及协方差矩阵，计算指定收益下方差最小的投资组合
#输出结果中port.sol$pw是资产配置权重
#例子如下：
df.yields <- GetGroupYields_combined(c("000001002","000010001","000028001",
                                       "000009002","000133001","000049001"),con.FOF)
averet <-  matrix(colMeans(df.yields[,2:8]),nrow=1)
rcov = cov(df.yields[,2:8])
target.return = 0.0006778961#0.15/250
port.sol = portfolio.optim(x = averet, pm = target.return,
                           covmat = rcov, shorts = F, reslow = rep(0,7), reshigh = rep(0.8,7))

#计算给定资产组合有效边界的函数
#输入值：averet是资产收益向量,例如：
#[,1]         [,2]    [,3]         [,4]        [,5]     [,6]         [,7]
#[1,] 0.0005650909 0.0007018182 0.00076 0.0006970909 0.001138182 0.000772 0.0006996364
#rcov是协方差矩阵
#nports是分解收益率的步长
#输出值：包含一系列有效组合的预期收益率和方差
#List of 2
#$ vol: num [1:1000] NA NA NA NA NA NA NA NA NA NA ...
#$ ret: num [1:1000] NA NA NA NA NA NA NA NA NA NA 
#函数主要思想：找出averet中绝对值最大的收益率a,
#以[-a,a]作为上下界，并且把这个连续区间分割成nports份，这样就得到了一系列离散的预取收益值
#对于每个预期收益值计算出最优组合的权重，并且求得此时组合的标准差
#对于无解的预期收益输出NA
effFrontier <- function (averet, rcov, nports = 20, shorts=T, wmax=1){
  mxret = max(abs(averet))
  mnret = -mxret
  n.assets = ncol(averet)
  reshigh = rep(wmax,n.assets)
  if( shorts )
  {
    reslow = rep(-wmax,n.assets)
  } else {
    reslow = rep(0,n.assets)
  }
  min.rets = seq(mnret, mxret, len = nports)
  vol = rep(NA, nports)
  ret = rep(NA, nports)
  for (k in 1:nports)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow,shorts=shorts),silent=T)
    if ( !is.null(port.sol) )
    {
      
      vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[k] = averet %*% port.sol$pw

    }
  }
  return(list(vol = vol, ret = ret))
}
#etest <- effFrontier(averet,rcov,shorts=F,nports = 1000)
#plot(etest$vol,etest$ret*250)

#计算有效边界组合中夏普最大的组合的投资比例
#optim.callback:根据资产收益率和协方差矩阵输出有效组合的夏普比率
#optimize：在给定区间内寻找某个函数的最小/最大值
#输出：最大夏普组合的权重
maxSharpe <- function (averet, rcov, shorts=T, wmax = 1){
  #给定目标收益率，输出该收益率下最优组合的夏普比率
  optim.callback = function(param,averet,rcov,reshigh,reslow,shorts)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=param, covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=shorts), silent = T)
    if (is.null(port.sol)) {
      #如果无解输出一个特别大的数
      ratio = 10^9
    } else {
      #否则计算该组合的负的夏普比率
      print(port.sol)
      m.return = averet %*% port.sol$pw
      print(m.return)
      m.risk = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      print(m.risk)
      ratio = -m.return/m.risk 
      print(ratio)
      assign("w",port.sol$pw,inherits=T)
    }
    return(ratio)
  }
  
  ef = effFrontier(averet=averet, rcov=rcov, shorts=shorts, wmax=wmax, nports = 1000)
  n = ncol(averet)
  reshigh = rep(wmax,n)
  if( shorts ) {
    reslow = -reshigh
  } else {
    reslow = rep(0,n)
  }
  max.sh = which.max(ef$ret/ef$vol)
  w = rep(0,ncol(averet))
  xmin = optimize(f=optim.callback, interval=c(ef$ret[max.sh-1], upper=ef$ret[max.sh+1]),
                  averet=averet,rcov=rcov,reshigh=reshigh,reslow=reslow,shorts=shorts)
  return(w)
}
#test <- maxSharpe(averet,rcov,shorts=F)

optMDD.gt3 <- function(x,ret)
{
  retu = ret %*% x
  obj = -maxDD(retu)
  weight.penalty = 100*(1-sum(x))^2
  #small.weight.penalty = 100*sum(x[x<0.001])
  small.weight.penalty = 0
  return( obj + weight.penalty + small.weight.penalty )
}

MaxDD.Opt <- DEoptim(optMDD.gt3,rep(0,0,7),rep(1,1,7),
              control=list(NP=2000,itermax=2000,F=0.2,CR=0.8),
              ret=as.matrix(df.yields[,2:8]))
