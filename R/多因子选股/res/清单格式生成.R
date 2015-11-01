stock<-read.csv("F:\\team4share\\实盘策略\\多因子选股模块\\计算\\327-410\\Alpha策略-王政1.csv",header = T)
stockc<-as.factor(stock[,1])
stockn<-as.factor('证券')
amount<-as.integer(0)
weight<-as.numeric(stock[,4])
data<-data.frame(证券代码=stock[,1],证券名称=stockn,目标数量=amount,目标权重=weight)

write.csv(data,"Z:\\实盘策略\\实盘记录\\zz8005-alpha2\\test.csv",row.names=F,quote = F)



