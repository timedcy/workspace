rm(list=ls())
setwd("G:\\R program&data\\final\\实盘记录\\2015-04-01")
###################################################################
a2<-read.csv("王政4-20150326-已平.csv",header=T)[,c("STOCK_CODE","STK_AMOUNT","STOP_PRICE")]
n=dim(a2)[1]
a2=a2[1:(n-1),]
for (j in 1:nrow(a2)){
  if(as.numeric(a2$code[j])<600000){
    a2$code[j]<-paste(a2$code[j],".SZ",sep="")
  }else{
    a2$code[j]<-paste(a2$code[j],".SH",sep="")  
  }
  a2$code[j]<-paste(paste(rep(0,9-nchar(a2$code[j])),
                              sep="",collapse=""),a2$code[j],sep="") 
}
write.csv(a1_327,file="王政4-20150326-已平w.csv",row.names=F)

##################################################################
a3<-read.csv("王政4-20150401-新开.csv",header=T)[,c("STOCK_CODE","STK_AMOUNT")]
a2_327<-merge(pingjun,a3,by.x="code",by.y="STOCK_CODE",all.y=T)

for (j in 1:nrow(a2_327)){
        if(as.numeric(a2_327$code[j])<600000){
                a2_327$code[j]<-paste(a2_327$code[j],".SZ",sep="")
        }else{
                a2_327$code[j]<-paste(a2_327$code[j],".SH",sep="")  
        }
        a2_327$code[j]<-paste(paste(rep(0,9-nchar(a2_327$code[j])),
                                    sep="",collapse=""),a2_327$code[j],sep="")
        
}
write.csv(a2_327,file="王政4-20150401-新开w.csv",row.names=F)
##################################################################

