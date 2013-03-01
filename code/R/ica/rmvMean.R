

###### center data for ICA - returns centered doc-term matrix

rmvMean <- function(dtm){
  print("centering data...")
  
  dtmMat <- as.matrix(dtm) 
  
  featureMeans <- colMeans(dtmMat)    # calculate mean value for each feature 
  newVectors <- t(apply(dtmMat, 1, "-", c(featureMeans))) # center data by subtracting mean of each feature for each document
  
  comb <- list(newVectors,featureMeans)
  
  
  return(comb) # returns doc-term matrix centered
}