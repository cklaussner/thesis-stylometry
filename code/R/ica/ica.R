library(matrixcalc)


ica <- function(pc,numOfIC){
  
  # normally pc should be doc-term matrix --TODO maybe put in a check
  
  
  #checking sizes
  #######################################
  msize <- size(pc)
  featureSize <- msize[1] 
  numSamples <- msize[2] 
  
  
  
  # setting some parameters
  epsilon <- 0.0001 # defining max.difference between new and old weight vector w 
  a1 <- 1    # sth. for nonlinearity calculation
  B <- matrix(0,nrow = featureSize,ncol=featureSize) # create matrix for saving ICs
  round = 1 # - will have as many rounds as one needs components
  
  
  while (round < numOfIC){   # keep estimating until desired no. of comp.
    
    
    w <- as.matrix(rnorm(featureSize))  # intitialise vector randomly to snd values
    
    w <- w - B%*%t(B)%*%w              # orthogonalize w.r.t. all other vectors 
    w <- w%/%spectral.norm(w)          #--
    
    
    
    wOld <- matrix(0,)
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}