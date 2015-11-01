#path 修改为储存的文件夹
rm(list=ls(all=TRUE));
setwd("Z:\\实盘策略\\实盘记录\\hs30080-alpha7\\2015-5-4")
#修改股票代码函数
tran=function(x) {
  n=length(x)
  xx=NULL
  for(i in 1:n) {
    a=x[i]
    if(a<510000) {
      m=nchar(a)
      m=6-m;
      if(m!=0){
        for(j in 1:m) {
          a=paste(0,a,sep="")
        }
      }    
      a=paste(a,".SZ",sep="")
    }
    if(a>=510000) {a=paste(a,".SH",sep="") }
    a=as.character(a)
    xx=c(xx,a)
  }
  return(xx)
}

#读入当日平开信息
origin<-read.csv(file="alpha7-已平组合.csv",stringsAsFactors=F)
futurename<-"IF1506"
future<-NULL

if(!is.na(origin[1,"REMARK"])){
  future<-origin[which(origin["STOCK_CODE"]==futurename),c("STOCK_CODE","STOP_PRICE","STK_AMOUNT")]
  future[,"STOCK_CODE"]<-paste(future[,"STOCK_CODE"],".CFE",sep="")
  direction<-"卖出"
  price<-"STOP_PRICE"
  name<-"已平-1"
  
  future<-cbind(future[,"STOCK_CODE"],future[,"STK_AMOUNT"],as.character(Sys.Date()),future[,"STOP_PRICE"],"期货","空平")
  
}else{
  hold<-origin[-which(origin["STOCK_CODE"]==futurename),c("STOCK_CODE","STK_AMOUNT")]
  hold[,"STOCK_CODE"]<-tran(hold[,"STOCK_CODE"])
  colnames(hold)<-c("trade_code","amount")
  if(!file.exists("hold")){
    dir.create("hold")
  }
  write.csv(hold,file=paste("hold/",Sys.Date(),"-hold.csv",sep=""),row.names=F,quote=F)
  
  if(is.element(1,origin[,"DK_TYPE"])){
      sign<-which(origin[,"DK_TYPE"]==1)
      mark<-origin[sign,c("STOCK_CODE","DETAIL_VALUE","STK_AMOUNT")]
      mark[,"STOCK_CODE"]<-tran(mark[,"STOCK_CODE"])
      write.table(mark, file = "未平股票.txt", row.names = F, quote = F) # 空格分隔
      origin<-origin[-sign,]    
    }
  
  future<-origin[which(origin["STOCK_CODE"]==futurename),c("STOCK_CODE","DETAIL_VALUE","STK_AMOUNT")]
  future[,"STOCK_CODE"]<-paste(future[,"STOCK_CODE"],".CFE",sep="")
  direction<-"买入"
  price<-"DETAIL_VALUE"
  name<-"新开"
  future<-cbind(future[,"STOCK_CODE"],future[,"STK_AMOUNT"],as.character(Sys.Date()),future[,"DETAIL_VALUE"],"期货","空开")
}

origin<-origin[-which(origin["STOCK_CODE"]==futurename),]
code<-as.numeric(origin[,"STOCK_CODE"])
code<-tran(code)
wind<-data.frame(code,origin[,"STK_AMOUNT"])
wind[,3]<-format(Sys.Date(), "%Y/%m/%d")
wind[,4]<-origin[,price]
wind[,5]<-"股票"
wind[,6]<-direction
colnames(wind)<-c("证券代码","买卖数量","买卖日期","买卖价格","证券类型","买卖方向")
colnames(future)<-c("证券代码","买卖数量","买卖日期","买卖价格","证券类型","买卖方向")
wind<-rbind(wind,future)
write.csv(wind,file=paste(Sys.Date(),"-",name,".csv",sep=""),row.names=F,quote=F)
