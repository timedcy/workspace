#计算月回报率
yld<-function(x,y=12){
  a<-length(x)
  (prod(1+x))^(y/a)-1
} 

#胜率
WR<-function(x){
    sum(x>yield_hs[names(x)])/length(x)  
}

cormat<-function(x){
  MAT<-0
  for (jj in chg_date){
    a<-fctdata[[jj]][,x]
    aa<-apply(a,2,rank)
    temp<-cor(aa)
    MAT<-MAT+temp
  }
  MAT<-MAT/length(chg_date)
}

scoring<-function(x){
  x_min<-min(x)
  x_max<-max(x)
  a<-(x-x_min)/(x_max-x_min)
  a<-floor(10*a)
  return(a)
}


factorpick1<-function(fct_name,chg_date,stock_pool){
  
  fct_evl<-matrix(NA,length(fct_name),8) #初始化一个矩阵（即new table->fct_evl）
  rownames(fct_evl)<-fct_name #行因子名
  colnames(fct_evl)<-c('decreasing','win_ratio','yield_y','WR_spr','yld_spr','yld_rho','yld_slp','fct_total')
  fct_evl<-as.data.frame(fct_evl) #转数据框
  
  lel<-min(length(stock_pool),10) #股票池 or 10 ->对股票分组计划为10组 此为避免股票只数少于10
  
  for (iii in fct_name){#对于每一个因子池里的因子循环以下步骤
    yield_10<-matrix(NA,lel,length(chg_date)) #声明矩阵 
    colnames(yield_10)<-chg_date   #赋列名--日期
    
    for (ii in chg_date){
      temp1<-fctdata[[ii]][c('trade_code',iii)] #某一交易日对应的 trade_code+ep (temp1-data.frame)
      temp1<-temp1[complete.cases(temp1[,2]),] #删除表中不存在ep值的
      rownames(temp1)<-temp1[,1] 
      #[...]->stockpool,temp1,dailydata的交集 取股票池中存在ep值 dailydata（close,adj,amt）的股票代码
      temp<-temp1[intersect(intersect(stock_pool,temp1[,1]),dailydata[[ii]][,'trade_code']),] #得到code+ep
      if (nrow(temp)>2){
        aaa<-temp[,1][order(temp[,2],decreasing = T)] #根据ep排序 降序（大1）
        n<-length(aaa) #股票只数
        m<-0
        ij<-chg_dateplus[which(chg_dateplus==ii)+1] #当前换仓日的下一个换仓日
        abcd<-dailydata[[ij]][,c('trade_code','close','adjfactor')]#下一个换仓日的数据
        defg<-dailydata[[ii]][,c('trade_code','close','adjfactor')]#当期换仓日的数据
        #向后复权价格：向后复权价格 = 原始价格 * 复权因子
        abcd['adjclose']<-abcd['close']*abcd['adjfactor'] 
        defg['adjclose']<-defg['close']*defg['adjfactor']
        rownames(abcd)<-abcd[,1]
        rownames(defg)<-defg[,1]
        #股票分段后 对应日期每组的增长率均值计算 完成表格yield_10
        for (j in 1:lel){
          aaaa<-aaa[m+1:round(n/(11-j))]          
          yield_10[j,ii]<-mean((abcd[aaaa,'adjclose'])/(defg[aaaa,'adjclose'])-1,na.rm = T)
          n<-n-length(aaaa)
          m<-m+length(aaaa)      
        }
        
      }            
    } 
    
    
    ##########################
    ######
    #############################
    fff<-complete.cases(t(yield_10))
    if(length(which(!fff))!=0){
      fff[1:max(which(!fff))]<-F
    } #找最大空值并赋之前的值F  
    
    #判断因子方向
    if(sum(fff)/length(fff)>0.3){
      yield_10<-yield_10[,fff]            
      #判断因子值最大的月回报-因子值最小的月回报
      if(yld(yield_10[1,])-yld(yield_10[lel,])>=0){
        gg<-1
        ggg<-'T'
      }else{
        gg<--1
        ggg<-'F'
      }
      
      fct_evl[iii,'yield_y']<-(yld(yield_10[1,])-yld(yield_10[lel,]))*gg 
      fct_evl[iii,'decreasing']<-gg
      fct_evl[iii,'win_ratio']<-WR((yield_10[1,]-yield_10[lel,])*gg)
      
      if(ggg=='F'){
        lm_ord<-1:lel
      }else{
        lm_ord<-lel:1
      }
      win_ratio<-apply(yield_10,1,WR)
      yield_y<-apply(yield_10,1,yld)
      fct_evl[iii,'WR_spr']<-cor(lm_ord,win_ratio,method = 'spearman')
      fct_evl[iii,'yld_spr']<-cor(lm_ord,yield_y,method = 'spearman')
      fct_evl[iii,'yld_rho']<-cor(lm_ord,yield_y)
      if(complete.cases(t(yield_y))){
        fct_evl[iii,'yld_slp']<-lm(yield_y~lm_ord)[[1]][2]      
      }
    }

                                
  }
  
  
  fct_evl<-fct_evl[complete.cases(fct_evl[,1]),]
  fct_evl_scl<-scale(fct_evl[,4:7])
  fct_total<-apply(fct_evl_scl[,1:3],1,sum)+3*fct_evl_scl[,4]
  fct_evl[,'fct_total']<-fct_total
  RRR<-fct_evl[order(fct_total,decreasing=T),]
  R<-0
  if (nrow(RRR)!=0){
    cormatrix<-cormat(rownames(RRR))
    ord<-c()
    ord[1]<-1
    jj<-2
    for(jjj in 2:nrow(cormatrix)){
      if(sum(abs(cormatrix[jjj,ord])>0.5)==0){
        ord[jj]<-jjj
        jj<-jj+1
      }    
    }
    RR<-RRR[ord,][1:min(10,length(ord)),c(1,4,5,6,7)]  
    score_mat<-apply(RR[,2:5],2,scoring)
    RR['score']<-apply(score_mat[,1:3],1,sum)+3*score_mat[,4]
    R<-RR[,c(1,6)]
    R['factor']<-rownames(R)
    R<-R[,c(3,1,2)] #factor,decreasing,score;
  }

  return(R)
}






