setwd("D:\\zdqh\\回测");
a=load("D:/zdqh/回测/2015-04-16--zz8005-79.RData")
b=load("D:/zdqh/回测/2015-04-24--zz8005-79.RData")
c=c(a,b)
stock_pick<-c
save(stock_pick,file=paste("D:\\zdqh\\回测","zz8005","-all-",79,".RData",sep="")) 
