library(matrixcalc)


ica <- function(pc,xwhiten,numOfIC){
  
  # normally pc should be term-doc matrix --TODO maybe put in a check
  
  
  #checking sizes
  #######################################
  msize <- dim(pc)
  featureSize <- msize[1] 
  numSamples <- msize[2] 
  
  
  
  # setting some parameters
  epsilon <- 0.0001 # defining max.difference between new and old weight vector w 
  a1 <- 1    # sth. for nonlinearity calculation
  B <- matrix(0,nrow = featureSize,ncol=featureSize) # create matrix for saving ICs
  round = 1 # - will have as many rounds as one needs components
  maxNumIteration <- 1000
  
  
  
  while (round < numOfIC){   # keep estimating until desired no. of comp.
    
    
    w <- as.matrix(rnorm(featureSize))  # intitialise vector randomly to snd values
    
    w <- w - B%*%t(B)%*%w              # orthogonalize w.r.t. all other vectors 
    w <- w%/%spectral.norm(w)          #--
    
    
    
    wOld <- matrix(0,dim(w)[1],dim(w)[2]) 
    wOld2 <- wOld
    
    
    i <- 1
    
    while (i < maxNumIteration){   # define max no. of dttempts to find component
      
      
      w <- w - B%*%t(B)%*%w              #project vector into space orthogonal to space spanned by earlier found basis vectors
      w <- w%/%spectral.norm(w)          
      
      if (spectral.norm(w - wOld) < epsilon | spectral.norm(w + wOld) < epsilon){  # essentially:if little diff. between w of last iter and now
        
        B[, round] <- w   # save new vector as final component
        # put here code for A mixing matrix if want to retrieve as well
        
        W[round, ] <- t(w)%*%xwhiten # calculate ICA filter???
        
           break
      } # ICA ready - do next one...
      
      wOld2 <- wOld
      wOld <- w
      
      hypTan <- tanh(a1*t(pc)*w)
      w <- (pc%*%hyptan-a1*t(sum(1-hypTan^2)%*%w)%/%numSamples) 
      
      w <- w%/%spectral.norm(w)
      i <- i+1
      
      
      
     }
    
    round <- round +1 
     
  }
  
   
  
}