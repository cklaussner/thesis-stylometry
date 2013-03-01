

################ calculates eigenvalues and eigenvectors from covariance matrix

pcamat <- function(vectors){
  
    wordMat = as.data.frame(vectors) 
  
    covMat <- cov(wordMat)  # create covariance matrix
    
    eigens <- eigen(covMat)   # Eigendecomposition
    
    evals <- eigens$values   # extract eigenvalues
    evecs <- eigens$vectors  # extract eigenvectors
    
    
    vals <- list(evals,evecs) 
    
    return(vals)
  
}