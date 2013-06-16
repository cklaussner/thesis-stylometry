#ICA semi-supervised --version 16 June
library(fastICA)
source("ICAfunctions.R")
source("functions.R")
source("RepDisComp.R")

# This is were we choose components with Representativeness-Distinctiveness

#source("evalICAsu.R")
#diff <- evalICAsu(nV,55,31)


evalICAsu <- function(dataset,noOfD,noOfnD){
  
  dataset <- as.matrix(check.data(dataset,noOfD,noOfnD)) # check that Dickens is first in set
  
  
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
    
    test.set <- as.matrix(S[i,]) # extract doc for test
    test.doc <- rownames(S)[i]
    train.set <- S[!rownames(S) %in% test.doc, ] # create new matrix with document left out
    
    
    #--- this is to know of what author there has been a reduction
    noOfD2 <- noOfD
    noOfnD2 <- noOfnD
    
    if (i <= noOfD){
      noOfD2 <- noOfD2-1
    }else{
      noOfnD2 <- noOfnD2-1
    }
    
    
    print(test.doc)
    # calculate author profile for train.set
    
    comp.List <- getDisTerm(A,dataset) # retrieve terms for each component
    
    alpha <- 1.1
    dis.Comp <- repDisComp(train.set,noOfD,noOfnD,alpha) # get discriminative components based on Repres.-Distinc. selection
    
    
    docLst <- getDisCompRD(train.set,noOfD2,noOfnD2,dis.Comp$features.1,dis.Comp$features.2) # simple retrieval of comp-doc weights for discriminatory components 
    docLst[[test.doc]] <- test.set
    
    docTopics <- combineWeights(dataset,comp.List,docLst) # term-in-document weight combination
    test.terms <- as.matrix(docTopics[[test.doc]]) # extract test doc
    docTopics[test.doc] <- NULL  # delete from train.set
    
    
    profiles <- getProfile(docTopics, 2.5) # get profiles for Dickens/ nonDickens, discard terms at mean by e.g. 2.5 
    D.profile <- as.matrix(profiles$D)
    nD.profile <- as.matrix(profiles$nD)
    
    
    # test histogram differences
    D.diff[[test.doc]] <- hist.diff(test.terms,D.profile)
    D.feat[[test.doc]] <- D.profile
    
    O.diff[[test.doc]] <- hist.diff(test.terms,nD.profile)
    O.feat[[test.doc]] <- nD.profile
    
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












