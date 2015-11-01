library(RODBC);
ch=odbcConnect('winddata',uid='root',pwd='123');
starttime="2005-01-01"
endtime="2015-04-14"
sql1=paste("select datetime,close from index1 where datetime>='",starttime,"'","and","datetime<='",endtime,"'")
sql2=paste("select datetime,close from index2 where datetime>='",starttime,"'","and","datetime<='",endtime,"'")
hs300index=sqlQuery(ch,sql1)
zz500index=sqlQuery(ch,sql2)
date=hs300index[,1]
index1=hs300index[,2]/hs300index[1,2];
index2=zz500index[,2]/zz500index[1,2];
a1=max(max(index1),max(index2))
a2=min(min(index1),min(index2))
t="hhhh"
plot(date,index1,type='l',col='red',xlab='Time',ylab='netvalue',ylim=c(a2,a1),main=t)
lines(date,index2,col='blue')
legend(x=date[1],y=a1,legend=c("zz500index","hs300index"),col=c("blue","red"),lty=c(1,1),cex=0.6)
