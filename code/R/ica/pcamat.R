

################ calculates eigenvalues and eigenvectors from covariance matrix

pcamat <- function(vectors){
  
  print("pca-ing...")
    wordMat <- as.matrix(vectors) 
  
    covMat <- cov(t(wordMat))  # create covariance matrix
    eigens <- eigen(covMat)   # Eigendecomposition
    
    evecs <- eigens$vectors  # extract eigenvectors
    eval <- eigens$val
  
    
  
    xS <- matrix(0,nrow =4, ncol=4)
    diag(xS) <- eval
    
    d <- solve(sqrt(xS))
    
    xwhiten <- as.matrix(d%*%t(evecs))
                  
    pc <- xwhiten%*%wordMat   # project new direction onto data 
  
    
   return(pc)
  
}

getwhiteningMatrix <- function() {
  return(whitenM)
}