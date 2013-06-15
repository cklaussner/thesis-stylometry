#ICA unsupervised --version 15 June
library(fastICA)
source("ICAfunctions.R")


evalICAu <- function(dataset,noOfD,noOfnD){
  
  numOfIC <-(dim(dataset))[1]-1
  
  # run ICA only once, since it's unsupervised this is ok!
  Y<- fastICA(dataset, numOfIC, alg.typ = "deflation",
              fun = "exp", alpha = 1.0, method = "R",
              row.norm = TRUE, maxit = 200, tol = 1e-04, verbose = TRUE,
              w.init = NULL)
  
  ########## preliminaries
  S <- Y$S 
  A <- Y$A
  
  numOfComp <- (dim(A))[1] 
  numOfTerms <- (dim(A))[2] 
  numOfDocs <- (dim(S))[1] 
  
  colnames(S) <- paste(1:numOfIC)
  rownames(A) <- paste(1:numOfIC)
  
  for(i in 1:numOfDocs{
    
    test.set <- as.matrix(S[i,]) # extract doc for test
    test.doc <- rownames(S)[i]
    train.set <- S[!rownames(S) %in% test.doc, ] # create new matrix with document left out
    
    # calculate author profile for train.set
    
    comp.List <- getDisTerm(A,dataset) # retrieve terms for each component
    
    dis.Comp <- getDisCompThres(train.set) # get discriminative components based on threshold
    
    docLst <- getDisCompComb(train.set,dis.Comp) # simple retrieval of comp-doc weights for discriminatory components 
    docLst[[test.doc]] <- test.set
    
    docTopics <- combineWeights(dataset,comp.List,docLst) # term-in-document weight combination
    test.terms <- docTopics[test.doc] # extract test doc
    docTopics[test.doc] <- NULL  # delete from train.set
    
    
    profiles <- getProfile(docTopics, 2.5) # get profiles for Dickens/ nonDickens, discard terms at mean by e.g. 2.5 
    D.profile <- as.matrix(profiles$D)
    nD.profile <- as.matrix(profiles$nD)
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
  
  

  

  
  
  
  
  
  
  
  
}