#ICA unsupervised -reversed input --version 16 June
library(fastICA)
source("ICAfunctions.R")
source("functions.R")


#source("evalICAu.R")
#diff <- evalICAu(nV,55,31)


evalICAu <- function(dataset,noOfD,noOfnD){
  
  dataset <- as.matrix(check.data(dataset,noOfD,noOfO)) # check that Dickens is first in set
  
  numOfIC <-(dim(dataset))[1]-1
  
  # run ICA only once, since it's unsupervised this is ok!
  Y<- fastICA(t(dataset), numOfIC, alg.typ = "deflation",
              fun = "exp", alpha = 1.0, method = "R",
              row.norm = TRUE, maxit = 200, tol = 1e-04, verbose = TRUE,
              w.init = NULL)
  
  ########## preliminaries
  S <- Y$S 
  A <- Y$A
  
  numOfDocs <- (dim(A))[2] 
  numOfComps <- (dim(A))[1] 
  numOfTerms <- (dim(S))[1] 
  
  colnames(S) <- paste(1:numOfIC)
  rownames(S) <- colnames(dataset)
  rownames(A) <- paste(1:numOfIC)
  colnames(A) <- rownames(dataset)
  #for saving profile-unseen hist. diff 
  D.diff <- list()
  O.diff <- list()
  
  #Dissim. Matrix
  sim <- list()
  
  #features
  D.feat <- list()
  O.feat <- list()
  inter.feat <- list()
  
  for(i in 1:numOfDocs){
    
    test.set <- as.matrix(A[,i]) # extract doc for test
    test.doc <- colnames(A)[i]
    train.set <- A[,!colnames(A) %in% test.doc ] # create new matrix with document left out
    
    print(test.doc)
    # calculate author profile for train.set
    
    comp.List <- getDisTerm(t(S),dataset) # retrieve terms for each component
    
    dis.Comp <- getDisCompThres(t(train.set)) # get discriminative components based on threshold
    
    docLst <- getDisCompComb(t(train.set),dis.Comp) # simple retrieval of comp-doc weights for discriminatory components 
    docLst[[test.doc]] <- test.set
    
    docTopics <- combineWeights(dataset,comp.List,docLst) # term-in-document weight combination
    test.terms <- as.matrix(docTopics[[test.doc]]) # extract test doc
    docTopics[test.doc] <- NULL  # delete from train.set
    
    
    profiles <- getProfile(docTopics, 2.5) # get profiles for Dickens/ nonDickens, discard terms at mean by e.g. 2.5 
    D.profile <- as.matrix(profiles$D)
    nD.profile <- as.matrix(profiles$nD)
    
    
    # test histogram differences
    D.diff[[test.doc]] <- hist.diff(test.terms,D.profile)
    D.feat[[test.doc]] <- as.matrix(D.profile[order(D.profile[,1],decreasing = TRUE ),])
    
    O.diff[[test.doc]] <- hist.diff(test.terms,nD.profile)
    O.feat[[test.doc]] <- as.matrix(nD.profile[order(nD.profile[,1],decreasing = TRUE ),])
    
    inter.features <- intersect(rownames(D.profile),rownames(nD.profile))
    inter.feat[[test.doc]] <- inter.features
    sim.DO <- dissim.Matrix(dataset,inter.features)
    cr <- getAdjRand(sim.DO, "complete", noOfD, noOfnD) # try with some other metric?
    sim[[test.doc]]<- sim.DO
    clust.eval[[test.doc]] <- cr
    
  }
  
  hist.results <- cv.results(D.diff,O.diff,clust.eval)
  featConsist <- featureConsistency(D.feat)
  featConsist.2 <- featureConsistency(O.feat)
  
  
  cross.val <- list()
  
  cross.val[["hist.res"]] <- hist.results
  cross.val[["sim"]] <- sim
  cross.val[["feat"]] <- inter.feat
  cross.val[["D.feat"]] <- D.feat
  cross.val[["O.feat"]] <- O.feat
  cross.val[["D.consist"]] <- featConsist
  cross.val[["O.consist"]] <- featConsist.2
  
  
  return(cross.val)
  
}












