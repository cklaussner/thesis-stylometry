#library(far)  


# not sure this is correct, but gives back right dim. so that's a start

## take mean-normalized vectors and perform svd - more stable way to do decorrelation than eigenvalue decomposition


svdMat <- function(nV){ # comes in format doc-term - needs to be transposed for svd
  
  print("svd-ing...")
  
  msize <- dim(nV)
  vectorSize <- msize[1] 
  numSamples <- msize[2] 
  
  nV <-t(nV)
  
  Vm <- t(nV)%*%nV    # calculate right singular vectors V = A^T *A 
  
  eigV <- eigen(t(Vm))  # calculate eigen components
  
  E<- eigV$vec  # get eigenvectors
  
  S <- eigV$val  # get eigenvalues
  S_sqrt <- sqrt(S)
  
  xS <- matrix(0,nrow =vectorSize, ncol=vectorSize)
  diag(xS) <- S_sqrt
  
  D <- solve(xS)
  whitenM <- D%*%E
  
  # whitening matrix
  
  A <- whitenM%*%t(nV)  # take transpose - we want V not V^T multiplied by doc-term matrix
  
  -------- # lose check to make sure the values off diagonal are 'small' enough
  #c <- cov(t(A))
  #d <- c > 0.00005
  #print("values over 0.00005...")
  #print(d)
  #dg <- diag(d)
  #print("diagonal elements...")
  #print(dg)
  ----------

  
  return(A) # return new data representation
  
}

getwhiteningMatrix <- function() {
  return(whitenM)
}

