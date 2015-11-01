library("WindR", lib.loc="C:/Program Files/R/R-3.1.1/library")
w.start()
w_tdays_data<-w.tdays("2015-03-27","2015-12-30")$Data[,1]
n<-length(w_tdays_data)
order<-seq(1,n,20)
tradeday<-w_tdays_data[order]
# w_tdays_data_next<-w.tdaysoffset(20,"2015-03-27")$Data
w.stop()
