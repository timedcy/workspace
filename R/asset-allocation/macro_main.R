setwd("C:/Users/liuli/workspace/asset-allocation")
source("macro_wind.R")
source("getdata.R")
source("dataproc.R")
library("MASS")

situation<-c("指标环比上升","指标环比下降","指标处于历史高位(高于半年均值)","指标处于历史低位(低于半年均值)","指标创新高(高于半年最高值)","指标创新低(低于半年最低值)")
sit<-factor(situation)

mark1<-matrix(nrow = nrow(data),ncol = ncol(data))
mark2<-matrix(nrow = nrow(data),ncol = ncol(data))
mark3<-matrix(nrow = nrow(data),ncol = ncol(data))

for(i in 1:ncol(data)){
  for(j in 2:nrow(data)){
    #指标环比
    if((!is.na(data[j,i]))&(!is.na(data[j-1,i]))){
      if(data[j,i]>data[(j-1),i]){
        mark1[j,i]<-1
      }else if(data[j,i]<data[(j-1),i]){
        mark1[j,i]<--1
      }
    }
    
    if(j>=6&(!is.na(data[j,i]))){
      if(!(is.na(data[(j-3),i])&is.na(data[(j-2),i]))){
        #指标历史高低位
        if(data[j,i]>mean(data[(j-5):(j-1),i],na.rm=T)){
          mark2[j,i]<-1
        }else if(data[j,i]<mean(data[(j-5):(j-1),i],na.rm=T)){
          mark2[j,i]<--1
        }
        #指标创新高
        if(data[j,i]>max(data[(j-5):(j-1),i],na.rm=T)){
          mark3[j,i]<-1
        }else if(data[j,i]<min(data[(j-5):(j-1),i],na.rm=T)){
          mark3[j,i]<--1
        }
      }
    }
  }
}

#从2005-01-31到2015-09-31
index<-data.frame(diff(log(hs300[36:165,2])),diff(log(zzbond[24:153,2])),zzcf[,3]/zzcf[,2])
temp<-apply(hs300_sub[36:165,2:11],2,log)
temp<-apply(hs300_sub[36:165,2:11],2,diff)
sub<-data.frame(temp,diff(log(zzbond3$Data[24:153,2])),diff(log(zzbond7$Data[24:153,2])),diff(log(zzbond10$Data[24:153,2])),diff(log(zzbond20$Data[24:153,2])),zzaf$Data[,3]/zzaf$Data[,2],zzmf$Data[,3]/zzmf$Data[,2],zzcpf$Data[,3]/zzcpf$Data[,2],zzef$Data[,3]/zzef$Data[,2])

p1<-matrix(nrow = 31,ncol = 3)
p2<-matrix(nrow = 31,ncol = 3)
p3<-matrix(nrow = 31,ncol = 3)
t1<-matrix(nrow = 31,ncol = 3)
t2<-matrix(nrow = 31,ncol = 3)
t3<-matrix(nrow = 31,ncol = 3)

filter<-function(mark,yield,p,t){
  for(i in 1:ncol(mark)){
    for(j in 1:ncol(yield)){
      test1<-yield[which(mark[,i]==1),j]
      test2<-yield[which(mark[,i]==-1),j]
      t[i,j]<-t.test(test1,test2)$statistic
      p[i,j]<-t.test(test1,test2)$p.value
    }
  }
}


#
corr<-matrix(nrow=ncol(data),ncol=ncol(index))
for(i in 1:ncol(data)){
  for(j in 1:ncol(index)){
    corr[i,j]<-cor.test(data[1:(nrow(data)-1),i],index[,j])$estimate
  }
}
test1<-yield[which(mark[,(i-1)]==1),j]
test2<-yield[which(mark[,(i-1)]==-1),j]
