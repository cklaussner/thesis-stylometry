
library(tm)  
library (plyr)
library(fastICA)
source("extractKeys.R")
source("RepDis.R")
keyAnalysis <- function(nV){ # nV is doc-by-term
  
  
# setting parameters
Msize <- dim(nV)
numOfDocs <- Msize[1] 
numOfTerms <- Msize[2] 
  
numOfIC <- (numOfDocs-1)   # set no. of comp.
keyThres <- 0.1
compThres <- 0.5
noOfD <- 55
noOfC <- 31

###perform ICA

Y<- fastICA(nV, numOfIC, alg.typ = "deflation",
            fun = "exp", alpha = 1.0, method = "C",   # R or C ???
            row.norm = TRUE, maxit = 200, tol = 1e-04, verbose = TRUE,
            w.init = NULL)

S <- abs(Y$S) # extract doc-by-comp 
A <- abs(Y$A) # get inverse of unmixing matrix W --> weights for terms of each comp.


##### choosing rep.- dis. components from ICA result

goodComp <- as.list(repDisComp(S,noOfD,noOfC))

#### filter for good comp.
S.red <- matrix(0,nrow=numOfDocs,length(goodComp))
A.red <- matrix(0,nrow=numOfTerms,length(goodComp))
At <- t(A)

for (i in 1:length(goodComp)){   # filter original S matrix for good components from Rep.- Dis.
  
  S.red[, i] <- S[ ,goodComp[[i]]]
  A.red[, i] <- t(At[ ,goodComp[[i]]])   # ordering has changed, so have to do A as well
}
 


doc.Topics <- extractKeys(nV,S,A,keyThres,2.0)

#red.Docs <- reduceKeys(doc.Topics,) only if no other selection I think

maxTerms <- collectMaxTerms(doc.Topics,100)  # take max no. of Terms and write to file, maybe have dynamic file naming

#writeKeys(doc.Topics)  only for every doc individually






}
