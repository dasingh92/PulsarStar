GroupedBarPlots_df <- function(dataFrame){
  df = data.frame()
  for (i in 1:dim(dataFrame)[2]) {
    if(data.class(dataFrame[,i]) != "factor" && data.class(dataFrame[,i]) != "character"){
      df = rbind(df, c(colnames(dataFrame)[i],(round(mean(dataFrame[dataFrame$target_class == "Not Pulsar",colnames(dataFrame)[i]]), 2)), 
                       "Not Pulsar"), stringsAsFactors = FALSE)
      df = rbind(df, c(colnames(dataFrame)[i],(round(mean(dataFrame[dataFrame$target_class == "Pulsar",colnames(dataFrame)[i]]), 2)),
                       "Pulsar"), stringsAsFactors = FALSE)
    }else{cat("skipping row",i, "as it is not numeric\n")}
  }
  
  df[,1] = as.factor(df[,1])
  df[,3] = as.factor(df[,3])
  df[,2] = as.numeric(df[,2])
  colnames(df) = c("Var1", "mean", "Var3")
  cat("Returning dataframe: \n",str(df))
  return(df)
}

GroupedBarPlots_df_sd <- function(dataFrame){
  df = data.frame()
  for (i in 1:dim(dataFrame)[2]) {
    if(data.class(dataFrame[,i]) != "factor" && data.class(dataFrame[,i]) != "character"){
      df = rbind(df, c(colnames(dataFrame)[i],(round(sd(dataFrame[dataFrame$target_class == "Not Pulsar",colnames(dataFrame)[i]]), 2)), 
                       "Not Pulsar"), stringsAsFactors = FALSE)
      df = rbind(df, c(colnames(dataFrame)[i],(round(sd(dataFrame[dataFrame$target_class == "Pulsar",colnames(dataFrame)[i]]), 2)),
                       "Pulsar"), stringsAsFactors = FALSE)
    }else{cat("skipping row",i, "as it is not numeric\n")}
  }
  
  df[,1] = as.factor(df[,1])
  df[,3] = as.factor(df[,3])
  df[,2] = as.numeric(df[,2])
  colnames(df) = c("Var1", "std", "Var3")
  cat("Returning dataframe: \n",str(df))
  return(df)
}

GroupedBarPlots_df_median <- function(dataFrame){
  df = data.frame()
  for (i in 1:dim(dataFrame)[2]) {
    if(data.class(dataFrame[,i]) != "factor" && data.class(dataFrame[,i]) != "character"){
      df = rbind(df, c(colnames(dataFrame)[i],(round(median(dataFrame[dataFrame$target_class == "Not Pulsar",colnames(dataFrame)[i]]), 2)), 
                       "Not Pulsar"), stringsAsFactors = FALSE)
      df = rbind(df, c(colnames(dataFrame)[i],(round(median(dataFrame[dataFrame$target_class == "Pulsar",colnames(dataFrame)[i]]), 2)),
                       "Pulsar"), stringsAsFactors = FALSE)
    }else{cat("skipping row",i, "as it is not numeric\n")}
  }
  
  df[,1] = as.factor(df[,1])
  df[,3] = as.factor(df[,3])
  df[,2] = as.numeric(df[,2])
  colnames(df) = c("Var1", "median", "Var3")
  cat("Returning dataframe: \n",str(df))
  return(df)
}


misclass.rate <- function(table){
  if(is.null(table)){
    print("ERROR!!! the input argument to the function is NULL")
  } else if(data.class(table) != "table"){
    print("ERROR!!! The input is not of the data class: table")
  } else{
    err = 0
    for (i in 1:dim(table)[2]) {
      err[i] = sum(table[,i]) - table[i,i]
    }
    return((sum(err)/sum(table)))
  }
}


