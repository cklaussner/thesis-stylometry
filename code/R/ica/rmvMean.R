

###### center data for ICA - returns what is put in :P - put in term-doc matrix; takes rowMeans, so for each feature

rmvMean <- function(dtm){
  print("centering data...")
  
  dtmMat <- as.matrix(dtm) 
  
  featureMean <<- rowMeans(dtmMat)    # calculate mean for each feature - word
  newVectors <- apply(dtmMat, 2, "-", c(featureMean)) # subtract respective row mean from each column
 
return(newVectors) # returns term-doc matrix centered
}


getFeatureMean <- function() {
  return(featureMean)
}