GetInvestmentStyle <-
  function(con.team,Group_id,Strategy_id)
  {
    # @author liuli
    
    # DESCRIPTION:
    # This function give the invesetment style based on position samples
    # using morning star style box method
    # size option: large,median,small
    # style option: value,blend,growth
    
    
    # Inputs:
    # con.team: connection of team database
    # Group_id
    # Strategy_id
    
    
    # Outputs:
    # A vector with portfolio's size and style. e.g. c("large","value")
    
    # FUNCTION:
    #获取所有的Group_id和Strategy_id对应的日期,股票代码和权重
    sql <- paste("SELECT Date,SecuCode,Wgt FROM GroupPositionSample WHERE Group_id =  ",Group_id," and Strategy_id = ",Strategy_id,sep = "")
    n <- length(Group_id)
    group <- list()
    result <- data.frame(Group_id=character(), Strategy_id=character(), date=character(),size=numeric(), size_weight=numeric(), value=numeric(), value_weight=numeric())
    index <- 0
    for (i in 1:n){
      group[[i]] <- sqlQuery(con.team, sql[i])
      group[[i]]$SecuCode = sprintf("%06d", as.numeric(group[[i]]$SecuCode))
    }
    #
    for (i in 1:n){
      group.sql1 <- sprintf("SELECT DISTINCT sizetype FROM StockStyleInfo WHERE securitycode = '%s'",as.character(group[[i]]$SecuCode))
      group.sql2 <- sprintf("SELECT DISTINCT valuetype FROM StockStyleInfo WHERE securitycode = '%s'",as.character(group[[i]]$SecuCode))
      sizetype <- c()
      valuetype <- c()
      for (j in 1:length(group.sql1)){
        sizetype <- c(sizetype, sqlQuery(con.team, group.sql1[j])$sizetype[1])
        valuetype <- c(valuetype, sqlQuery(con.team, group.sql2[j])$valuetype[1])
      }
      group[[i]]$sizetype <- sizetype
      group[[i]]$valuetype <- valuetype
      date <- unique(group[[i]]$Date)
      for (j in 1:length(date)){
        index <- index + 1
        group.temp <- group[[i]][which(group[[i]]$Date == date[j]),]
        sum_sizetype <- c(sum(group.temp[which(sizetype == 1),3]), group.temp[which(sizetype == 2),3], group.temp[which(sizetype == 3),3])
        size <- which.max(sum_sizetype)
        size_weight <- sum_sizetype[size]/sum(sum_sizetype)
        sum_valuetype <- c(sum(group.temp[which(valuetype == 1),3]), group.temp[which(valuetype == 2),3], group.temp[which(valuetype == 3),3])
        value <- which.max(sum_valuetype)
        value_weight <- sum_valuetype[value]/sum(sum_valuetype)
        result[index,] <-c(Group_id[i], Strategy_id[i], date[j], size, size_weight, value, value_weight)
      }
      
    }
    return(result)
  }