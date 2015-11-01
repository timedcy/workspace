###从母基金数据库中选取制定团队
###分析这些团队的最优组合比例
###以及确定比例之后的组合曲线
###卫海天 20150319

require(tseries)
require(DEoptim)
#source("./Position Analysis.R")
source("./FOF_Performance_Ann.R")

#配置数据库连接
drv1 <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "C:/Users/xhth/Documents/sqljdbc4-3.0.jar") 
con.factor <<- dbConnect(drv1, "jdbc:sqlserver://jydb", "user", "kgo888")
con.juyuan <<- dbConnect(drv1, "jdbc:sqlserver://jydb", "xhth", "zzs2012")
con.FOF <<- dbConnect(drv, "jdbc:sqlserver://192.168.150.13", "zhongzixing", "xing2015@")
#con.FOF <<- dbConnect(drv1, "jdbc:sqlserver://jydb", "jinshan", "kgofactor2014")

##把指定团队的指定策略的日收益率从母基金数据库中提取出来
##并按时间顺序整理在一张表中
teamlist <- c("000001002","000010001","000028001",
              "000009002","000133001","000049001")
df.yields <- GetGroupYields_combined(teamlist,con.FOF)
#样例数据如下：
#         Date     000028001    000010001 000038001    000012003    000036001     000041001    000040001
# 1 2012-01-04  0.0043240296  0.006708804   -0.0078  0.000000000  0.011138828 -3.017535e-03 -0.001186374
# 2 2012-01-05 -0.0052361131 -0.003386170   -0.0232 -0.012230419 -0.007774056 -2.353437e-03  0.002296965

#提取基本数据：收益均值和协方差
averet <-  matrix(colMeans(df.yields[,2:7]),nrow=1)
rcov = cov(df.yields[,2:7])

#绘制组合的有效边界
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
etest <- effFrontier(averet,rcov,shorts=F,nports = 1000)
plot(etest$vol,etest$ret*250)

#计算给定比率的组合
weight <- rep(1/6,6)
df.yield.fixed <- GetWeightedYield(df.yields,weight)

#计算给定目标收益的组合比率
target.return = 0.18/250
port.sol = portfolio.optim(x = averet, pm = target.return,
                           covmat = rcov, shorts = F, reslow = rep(0.1,6), reshigh = rep(0.8,6))
target.weight <- port.sol$pw
df.yields.target <- GetWeightedYield(df.yields,target.weight)

#计算最大夏普组合比率
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
msharp.weight <- maxSharpe(averet,rcov,shorts=F)
df.yields.maxsharp <- GetWeightedYield(df.yields,as.vector(msharp.weight))

#计算最小回撤组合
optMDD.gt3 <- function(x,ret){
  retu = ret %*% x
  obj = -maxDD(retu)
  weight.penalty = 100*(1-sum(x))^2
  #small.weight.penalty = 100*sum(x[x<0.001])
  small.weight.penalty = 0
  return( obj + weight.penalty + small.weight.penalty )
}
MaxDD.Opt <- DEoptim(optMDD.gt3,rep(0.1,6),rep(1,6),
                     control=list(NP=2000,itermax=1000,F=0.2,CR=0.8),
                     ret=as.matrix(df.yields[,2:7]))
MaxDD.weight <- MaxDD.Opt$optim$bestmem
df.yields.maxdd <- GetWeightedYield(df.yields,MaxDD.weight)
##计算指定回撤水平的组合比率
MaxDD.Opt$member$bestvalit[ abs(MaxDD.Opt$member$bestvalit - 0.028) < 0.001]
index <- which(abs(MaxDD.Opt$member$bestvalit - 0.028) < 0.001)[1]
MaxDD.target.weight <- MaxDD.Opt$member$bestmemit[index,]
df.yields.target.maxdd <- GetWeightedYield(df.yields,MaxDD.target.weight)


#根据不同的比例查看组合后的整体效果
report_portfolio <- function(df.yield){
  df.yields$Date <- as.Date(df.yields$Date)
  input <- list(Portfolio=df.yield,id="Portfolio")
  return2pdf_FOF_Combined_YieldReport(input)
}
report_portfolio(df.yield.fixed)



