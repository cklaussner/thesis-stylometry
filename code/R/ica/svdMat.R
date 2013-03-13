#library(far)  


# not sure this is correct, but gives back right dim. so that's a start

## take mean-normalized vectors and perform svd - more stable way to do decorrelation than eigenvalue decomposition


svdMat <- function(nV){ # comes in format term-doc - needs to be transposed for svd 
  
  print("svd-ing...")
  
  
  
  msize <- dim(nV)
  featureSize <- msize[1] 
  numSamples <- msize[2] 
 
  
  print("svd-ing...")
  Vm <- t(nV)%*%nV    # calculate right singular vectors V = A^T *A 
   
  eigV <- eigen(Vm)  # calculate eigen components
  
     
     V <- eigV$vec  # get eigenvectors
   
     #orthV <- orthonormalization(V,basis = TRUE,norm = TRUE) # Gram-Schmidt Orthnormalization
     
    #V <- orthV
     A <- t(V)%*%t(nV)  # take transpose - we want V not V^T multiplied by term-doc matrix
  
    whitenM <- t(V) # whitening matrix
    
    return(A) # return new data representation 

  
  
  
  
  
  -------- # lose check to make sure the values off diagonal are 'small' enough
  #c <- cov(t(A))
  #d <- c > 0.00005
  #print("values over 0.00005...")
  #print(d)
  #dg <- diag(d)
  #print("diagonal elements...")
  #print(dg)
  ----------

  
 
  
}

getwhiteningMatrix <- function() {
  return(whitenM)
}

