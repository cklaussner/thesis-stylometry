

###### center data for ICA - returns centered doc-term matrix

rmvMean <- function(dtm){
  print("centering data...")
  
  dtmMat <- as.matrix(dtm) 
  
  signalMean <<- rowMeans(dtmMat)    # 
  newVectors <- apply(dtmMat, 2, "-", c(signalMean)) # 
  
  
  return(newVectors) # returns doc-term matrix centered
}


getSignalMean <- function() {
  return(signalMean)
}