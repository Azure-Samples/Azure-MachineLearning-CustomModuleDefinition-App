merge2Datasets <- function(dataframe1, dataframe2, swap = TRUE){
  if(swap){
    return(rbind(dataframe1, dataframe2))
  }
  else{
    return(rbind(dataframe2, dataframe1))
  }
}

merge3Datasets <- function(dataframe1, dataframe2, dataframe3){
  return(rbind(rbind(dataframe1, dataframe2), dataframe3))
}