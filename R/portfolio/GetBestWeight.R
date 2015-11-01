require(tseries)
require(DEoptim)
source("GerCombineYield.R")
#source("./Position Analysis.R")
source("./FOF_Performance_Ann.R")

#计算给定资产组合有效边界的函数
#输入值：averet是资产收益向量,例如：
#[,1]         [,2]    [,3]         [,4]        [,5]     [,6]         [,7]
#[1,] 0.0005650909 0.0007018182 0.00076 0.0006970909 0.001138182 0.000772 0.0006996364
#rcov是协方差矩阵
#nports是分解收益率的步长
#输出值：包含一系列有效组合的预期收益率和方差
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


#计算有效边界组合中夏普最大的组合的投资比例
#optim.callback:根据资产收益率和协方差矩阵输出有效组合的夏普比率
#optimize：在给定区间内寻找某个函数的最小/最大值
#输出：最大夏普组合的权重
maxSharpe <- function (averet, rcov, shorts=T, wmax = 1){
  #给定组合权重，输出该权重下的夏普比率
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


optMDD.gt3 <- function(x,ret)
{
  retu = ret %*% x
  obj = -maxDD(retu)
  weight.penalty = 100*(1-sum(x))^2
  #small.weight.penalty = 100*sum(x[x<0.001])
  small.weight.penalty = 0
  return( obj + weight.penalty + small.weight.penalty )
}


GetBestWeight <-
  function(con.team,strategyList,Method)
  {
    # @author liuli
    
    # DESCRIPTION:
    # This function gives the best weight for combining different assets
    # based on different method
    
    # Inputs:
    # con.team: connection of team database
    # strategyList: a list with the format groupid+strategyid.e.g. c("000001001","000002001")
    # Method: sharpe: best sharpe strategy
    #         maxdd : return best weights based on target max drawdown
    #         blacklitter: weights based on blacklitterman model
    
    
    # Outputs:
    # a vector with weights for each team,with name being the same as strategyList
    
    # FUNCTION:
    yield.data <- GerCombineYield(con.juyuan,con.team,strategyList,Method)
    n <- dim(yield.data)[2]
    averet <- matrix(colMeans(yield.data[,1:n]),nrow=1)
    rcov <- cov(yield.data[,1:n])

    if (Method == "sharpe") {
      yield.weight <- maxSharpe(averet, rcov, shorts=T, wmax = 1)
    } else if (Method == "maxdd"){
      MaxDD.Opt <- DEoptim(optMDD.gt3,rep(0,0,n),rep(1,1,n),
              control=list(NP=2000,itermax=2000,F=0.2,CR=0.8),
              ret=as.matrix(yield.data[,1:n]))
      MaxDD.weight <- MaxDD.Opt$optim$bestmem
      df.yields.maxdd <- GetWeightedYield(df.yields,MaxDD.weight)
    } else {
      
    }
    
  }