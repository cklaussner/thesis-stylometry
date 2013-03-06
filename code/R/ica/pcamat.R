

################ calculates eigenvalues and eigenvectors from covariance matrix

pcamat <- function(vectors){
  
  print("pca-ing...")
    wordMat <- as.data.frame(vectors) 
  
    covMat <- cov(wordMat)  # create covariance matrix
    
    eigens <- eigen(covMat)   # Eigendecomposition
    
    
    evecs <- eigens$vectors  # extract eigenvectors
    
    pc <- t(evecs)%*%t(wordMat)   # project new direction onto data 
  
    
    c <- cov(t(pc))
    d <- c > 0.00005
    print("values over 0.00005...")
    print(d)
    dg <- diag(d)
    print("diagonal elements...")
    print(dg)
    
    
    return(pc)
  
}