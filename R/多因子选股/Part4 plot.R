plot_cal2<-function(capital,date,t='the trend of revenue',rho=0.03,cex=0.7,col='black'){
  # redata<-cumprod(data+1)
  n<-length(capital)
  draw<-vector(length=n)
  
  date<-as.Date(date)
  datenum<-as.numeric(date)
  
  yield<-diff(capital)/capital[1:(n-1)]
  
  for(i in 1:n){
    draw[i]<-(max(capital[1:i])-capital[i])/max(capital[1:i])
  }
  drawdown<-round(max(draw),3)
  point<-which(draw==max(draw))
  point<-date[point]
  vol<-round(sd(yield)*sqrt(365/((datenum[n]-datenum[1])/(n+1))),3)
  yearreturn<-round((capital[n]/capital[1])^(365/(datenum[n]-datenum[1]))-1,3)
  sharprate<-round((yearreturn-rho)/(vol),3)
  reva<-paste("Annual revenue= ",paste(yearreturn*100,"%",sep=""),"fluctuation ratio=",paste(vol*100,"%",sep=""))
  drsp<-paste("Sharpe Ratio=",sharprate,"The maximum retracement=",paste(drawdown*100,"%",sep=""))
  points<-paste("The maximum retracement points",point)
  ratio=paste("The ratio of annual revenue and max retracement=",round(yearreturn/drawdown,3))
  
  library(RODBC);
  ch=odbcConnect('winddata',uid='root',pwd='123');
  sql1=paste("select close from index1 where datetime>='",date[1],"'","and","datetime<='",date[n],"'")
  hs300index=sqlQuery(ch,sql1)
  hs300index=hs300index[,1]/hs300index[1,1];
  a1=max(max(capital),max(hs300index))
  a2=min(min(capital),min(hs300index))
  
  qualmax<-a1
  qual098<-qualmax*0.95
  qual09<-qualmax*0.9
  qual=qualmax*0.85;
  hrz=median(datenum)
  
  n=length(date)
  date1=date[2:n]
  if(date[1]<"2005-01-01") {linshi1=date1}
  else {linshi1=date}
  plot(date,capital,type='l',col='red',xlab='Time',ylab='netvalue',ylim=c(a2,a1),main=t)
  lines(linshi1,hs300index,col='blue')
  legend(x=date[1],y=a1,legend=c("hs300","stock_pick"),col=c("blue","red"),lty=c(1,1),cex=0.6)
  text(hrz,qualmax,reva,cex=cex,col=col)
  text(hrz,qual098,drsp,cex=cex,col=col)
  text(hrz,qual09,points,cex=cex,col=col)
  text(hrz,qual,ratio,cex=cex,col=col)
  print(reva);
  print(drsp);
  print(points);
  print(ratio);
  
}
