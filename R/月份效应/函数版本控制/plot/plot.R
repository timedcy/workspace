####
plot_cal<-function(capital,date,t='the trend of revenue',rho=0.03,cex=0.7,col='black'){
  #data和date的长度得一样
  
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
  qualmax<-max(capital)
  qual098<-qualmax*0.95
  qual09<-2*qual098-qualmax
  qual=qualmax*0.85;
  hrz=median(datenum)
  
  plot(date,capital,type='l',col='blue',xlab='Time',ylab='netvalue',main=t)
  text(hrz,qualmax,reva,cex=cex,col=col)
  text(hrz,qual098,drsp,cex=cex,col=col)
  text(hrz,qual09,points,cex=cex,col=col)
  text(hrz,qual,ratio,cex=cex,col=col)
  
}
