library(riv)
library(MASS)

#### calculates the whitening  - doesn't work yet

whitenv <- function(vectors, evals, evecs){
  
  eval <- sqrt(as.complex(evals))
  
  whitMat <-  ginv(as.vector(eval)) * t(evecs)    # not sure this will give right result / #####TODO try svd - compare to Matlab output 
  
 
  newVectors  <- apply(whitMat,2,"*", vectors) # find right method to calculate - this probably not right dims
  
}