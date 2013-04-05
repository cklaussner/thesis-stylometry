


keyAnalysis <- function(nV) # nV is doc-by-term
  
  
  # setting parameters
Msize <- dim(nV)
numOfDocs <- Msize[1] 
#numOfTerms <- Msize[2] 
  
numOfIC <- (numOfDocs-1)   # set no. of comp.
keyThres <- 0.1
compThres <- 2.0


###perform ICA

Y<- fastICA(nV, numOfIC, alg.typ = "deflation",
            fun = "exp", alpha = 1.0, method = "R",
            row.norm = TRUE, maxit = 200, tol = 1e-04, verbose = TRUE,
            w.init = NULL)

S <- Y$S # extract doc-by-comp 
A <- (Y$A) # get inverse of unmixing matrix W --> weights for terms of each comp.


##### choosing rep.- dis. components from ICA result
 goodComp <- as.list(repDisComp(S,noOfD,noOfC))

#### filter for good comp.
S.red <- matrix(0,nrow=numOfDocs,length(goodComp))

for (i in 1:length(goodComp)){   # filter original S matrix for good components from Rep.- Dis.
  
  S.red[, i] <- S[ ,goodComp[i]]
}




