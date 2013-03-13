library(MASS)

################ calculates eigenvalues and eigenvectors from covariance matrix

pcamat <- function(vectors){   # input is term-doc matrix
  
  msize <- dim(vectors)
  featureSize <- msize[1] 
  numSamples <- msize[2] 
  
  print("pca-ing...")
    wordMat <- as.matrix(vectors) 
  
    covMat <- cov(t(wordMat))  # create covariance matrix
    eigens <- eigen(covMat)   # Eigendecomposition
    
    vec <- eigens$vec  # extract eigenvectors
    eval <- eigens$val # extract eigenvalues
  
    
    xs <- matrix(0,nrow=featureSize,ncol=featureSize)
    diag(xs) <- eval
    xss = ginv(sqrt(xs))    # take inverse of unit variance of eigenvalues
  

    xwhiten <- as.matrix(xss%*%t(vec))
                  
    pc <- xwhiten%*%wordMat   # project new direction onto data 
  
    
   return(pc)
  
}

getwhiteningMatrix <- function() {
  return(whitenM)
}