GetCombineYield <-
  function(con.juyuan,con.team,strategyList,Method)
  {
    # @author liuli
    
    # DESCRIPTION:
    # This function extract daily return of several teams from database
    
    # Inputs:
    # con.juyuan: connection of juyuan database,used to calculate index data
    # con.team: connection of team database
    # strategyList: a list with the format groupid+strategyid.e.g. c("000001001","000002001")
    # Method: L:keep the longest yield record,replace NA with 0
    #         S:keep the shortest yield record,remove all NA
    
    
    # Outputs:
    # an xts object of daily yields of team yields,with name being the same as strategyList
    
    # FUNCTION:
    
    library(reshape)
    library(xts)
    #Get groupid and Stratege_id
    group_id <- c()
    strategy_id <- c()
    for(i in 1:length(strategyList)){
      group_id[i] <- substr(strategyList[i],1,6)
      strategy_id[i] <- substr(strategyList[i],7,9)
    }
    sql<-paste("select Date,Daily_yield from GroupYield where Group_id = ", group_id," and Strategy_id = ",strategy_id,sep="")
    
    yield <- list()
    for(i in 1:length(sql)){
      yield[[i]]<- sqlQuery(con.team, sql[i])
    }
    
    len <- c()
    for(i in 1:length(strategyList)){
      len[i] <- length(yield[[i]]$Date)
      yield[[i]] <- rename(yield[[i]], c(Daily_yield = strategyList[i]))  
    }
    
    #Method L or S  
    if(Method == "L"){
      data <- yield[[which.max(len)]]
      for(i in 1:length(strategyList)){
        if(!i == which.max(len)){
          data <- merge(data, yield[[i]], all.x = TRUE)
        }
      }
      data[is.na(data)] <- 0
      data <- xts(data[,2:dim(data)[2]], order.by = as.Date(data[,1]))
      return(data)
    }else{
      data <- yield[[which.min(len)]]
      for(i in 1:length(strategyList)){
        if(!i == which.min(len)){
          data <- merge(data, yield[[i]], all.x = TRUE)
        }
      }
      data <- na.omit(data)
      data <- xts(data[,2:dim(data)[2]], order.by = as.Date(data[,1]))
      return(data)
    }
    
  }
