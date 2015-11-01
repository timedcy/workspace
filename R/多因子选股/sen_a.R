sensibility_analysis=function(stock_pickn,topn,beta,name="allA_zz500weight_zz500ic",impact=0.004,lastday="2015-06-03",zz500ic="zz500ic0930") {
  output=data.frame();
  p1=0.64
  p2=0.16
  p3=0.2
  for (k in 1:length(stock_pickn))  {
    stock_pick=stock_pickn[[k]];
    caplist1=Backtesthedge5(30000000,0.80,beta,stock_pick,0.00035,0.00003,0.001,impact,0.00017,lastday,0.05,zz500ic)
    caplist1=caplist1[[1]]
    capital=caplist1[,2]/caplist1[1,2]
    date=caplist1[,1]
    
    capital_1<-NULL
    capital_2<-NULL
    capital_3<-NULL
    date1<-NULL
    date2<-NULL
    date3<-NULL
    
    l1=p1*length(capital)
    l2=p2*length(capital)
    l3=p3*length(capital)
    
    t=1
    while(t<=l1)
    {
      capital_1[t]=capital[t]
      date1[t]=date[t]
      t=t+1
    }
    
    tt=l1+1
    while(tt<=l1+l2)
    {
      capital_2[tt-l1]=capital[tt]
      date2[tt-l1]=date[tt]
      tt=tt+1
    }
    
    ttt=l1+l2+1
    while(ttt<=length(capital))
    {
      capital_3[ttt-l1-l2]=capital[ttt]
      date3[ttt-l1-l2]=date[ttt]
      ttt=ttt+1
    }
    
    rho=0.03;
    
    n1<-length(capital_1)
    draw1<-vector(length=n1)
    date1<-as.Date(date1)
    datenum1<-as.numeric(date1)
    yield1<-diff(capital_1)/capital_1[1:(n1-1)]
    for(i in 1:n1){
      draw1[i]<-(max(capital_1[1:i])-capital_1[i])/max(capital_1[1:i])
    }
    drawdown1<-round(max(draw1),3)  ###the max re...
    point1<-which(draw1==max(draw1)) 
    point1<-date[point1] ##the max re.. date
    vol1<-round(sd(yield1)*sqrt(365/((datenum1[n1]-datenum1[1])/(n1+1))),3)
    yearreturn1<-round((capital_1[n1]/capital_1[1])^(365/(datenum1[n1]-datenum1[1]))-1,3)
    sharprate1<-round((yearreturn1-rho)/(vol1),3)
    calmar1=round(yearreturn1/drawdown1,3)
    
    #yearreturn=paste(yearreturn*100,"%",sep="")
    vol1=paste(vol1*100,"%",sep="")
    drawdown1=paste(drawdown1*100,"%",sep="")
    l=k
    output[l,"stock_pick1"]=name
    output[l,"top1"]=topn[k];
    output[l,"beta1"]=beta;
    output[l,"yearreturn1"]=yearreturn1;
    output[l,"max_retracement1"]=drawdown1;
    output[l,"calmar_ratio1"]=calmar1;
    output[l,"sharprate_ratio1"]=sharprate1;
    output[l,"vol1"]=vol1;
    output[l,"max_retracement_point1"]=as.character(point1);
    
    
    n2<-length(capital_2)
    draw2<-vector(length=n2)
    date2<-as.Date(date2)
    datenum2<-as.numeric(date2)
    yield2<-diff(capital_2)/capital_2[1:(n2-1)]
    for(i in 1:n2){
      draw2[i]<-(max(capital_2[1:i])-capital_2[i])/max(capital_2[1:i])
    }
    drawdown2<-round(max(draw2),3)  ###the max re...
    point2<-which(draw2==max(draw2)) 
    point2<-date[point2] ##the max re.. date
    vol2<-round(sd(yield2)*sqrt(365/((datenum2[n2]-datenum2[1])/(n2+1))),3)
    yearreturn2<-round((capital_2[n2]/capital_2[1])^(365/(datenum2[n2]-datenum2[1]))-1,3)
    sharprate2<-round((yearreturn2-rho)/(vol2),3)
    calmar2=round(yearreturn2/drawdown2,3)
    
    #yearreturn=paste(yearreturn*100,"%",sep="")
    vol2=paste(vol2*100,"%",sep="")
    drawdown2=paste(drawdown2*100,"%",sep="")
    l=k
    output[l,"stock_pick2"]=name
    output[l,"top2"]=topn[k];
    output[l,"beta2"]=beta;
    output[l,"yearreturn2"]=yearreturn2;
    output[l,"max_retracement2"]=drawdown2;
    output[l,"calmar_ratio2"]=calmar2;
    output[l,"sharprate_ratio2"]=sharprate2;
    output[l,"vol2"]=vol2;
    output[l,"max_retracement_point2"]=as.character(point2);
    
    n3<-length(capital_3)
    draw3<-vector(length=n3)
    date3<-as.Date(date3)
    datenum3<-as.numeric(date3)
    yield3<-diff(capital_3)/capital_3[1:(n3-1)]
    for(i in 1:n3){
      draw3[i]<-(max(capital_3[1:i])-capital_3[i])/max(capital_3[1:i])
    }
    drawdown3<-round(max(draw3),3)  ###the max re...
    point3<-which(draw3==max(draw3)) 
    point3<-date[point3] ##the max re.. date
    vol3<-round(sd(yield3)*sqrt(365/((datenum3[n3]-datenum3[1])/(n3+1))),3)
    yearreturn3<-round((capital_3[n3]/capital_3[1])^(365/(datenum3[n3]-datenum3[1]))-1,3)
    sharprate3<-round((yearreturn3-rho)/(vol3),3)
    calmar3=round(yearreturn3/drawdown3,3)
    
    #yearreturn=paste(yearreturn*100,"%",sep="")
    vol3=paste(vol3*100,"%",sep="")
    drawdown3=paste(drawdown3*100,"%",sep="")
    l=k
    output[l,"stock_pick3"]=name
    output[l,"top3"]=topn[k];
    output[l,"beta3"]=beta;
    output[l,"yearreturn3"]=yearreturn3;
    output[l,"max_retracement3"]=drawdown3;
    output[l,"calmar_ratio3"]=calmar3;
    output[l,"sharprate_ratio3"]=sharprate3;
    output[l,"vol3"]=vol3;
    output[l,"max_retracement_point3"]=as.character(point3);
    
    
  
    
    
    dd=paste(name,"--",topn[k],"--",beta,sep="")
    plot_cal2(caplist1[,2]/caplist1[1,2],caplist1[,1],t=dd);
  }
  output[,"1y"]=scale(output[,"yearreturn1"], center=T,scale=T)
  output[,"1c"]=scale(output[,"calmar_ratio1"], center=T,scale=T)
  output[,"1s"]=scale(output[,"sharprate_ratio1"], center=T,scale=T)
  output[,"2y"]=scale(output[,"yearreturn2"], center=T,scale=T)
  output[,"2c"]=scale(output[,"calmar_ratio2"], center=T,scale=T)
  output[,"2s"]=scale(output[,"sharprate_ratio2"], center=T,scale=T)                  
  output[,"3y"]=scale(output[,"yearreturn2"], center=T,scale=T)
  output[,"3c"]=scale(output[,"calmar_ratio2"], center=T,scale=T)
  output[,"3s"]=scale(output[,"sharprate_ratio2"], center=T,scale=T) 
  
  
  return(output)
}
