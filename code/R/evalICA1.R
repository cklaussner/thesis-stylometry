
source("ICA1.R")

# Evaluation of ICA pure and simple 


evalICA1 <- function(nV,numOfIC){
  
  
  Y <- ICA1(nV,numOfIC) # run ICA once
  
  
  # for all documents in set 
  #exclude one document at a time
  
  #---term-document
  # take mean value for each feature over one author's set  
  # keep n terms after threshold alpha
  # compute histogram for new combined set (absolute values of term weights)
  
  # compute histogram for doc left out (absolute values of term weights)
  
  # compare histograms
  # compare to non-author documents
  # compare different profiles
  
  
  
  
} 
