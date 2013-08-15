#ICA unsupervised 5 fold --version 21 June
library(fastICA)
source("ICAfunctions.R")
source("functions.R")



#source("evalICAu5.R")
#diff <- evalICAu5(dataset,45,29)


evalICAu5 <- function(dataset,noOfD,noOfnD){
  
  dataset <- as.matrix(check.data(dataset,noOfD,noOfnD)) # check that Dickens is first in set
  
  
  numOfIC <-(dim(dataset))[1]-1
  numOfIC <- 47
  
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
  rownames(S) <- rownames(dataset)
  rownames(A) <- paste(1:numOfIC)
  colnames(A) <- colnames(dataset)
  
  
  #for saving profile-unseen hist. diff 
  D.diff <- list()
  O.diff <- list()
  
  hist <- list()
  hist2 <- list()
  
  #Dissim. Matrix
  sim <- list()
  clust.eval <- list()
  
  #features
  D.feat <- list()
  O.feat <- list()
  inter.feat <- list()
  D.orig <- list()
  nD.orig <- list()
  
  iterations <- as.integer(numOfDocs/5)
  i <- 1
  i2<- 1+4
  
  
  for (l in 1:iterations){
    
    print(l)
    
    test.set <- as.matrix(S[i:i2,]) # extract doc for test
    test.doc <- rownames(S)[i:i2]
    train.set <- S[!rownames(S) %in% test.doc, ] # create new matrix with document left out
    
    
    #--- this is to know of what author there has been a reduction
    di <- 0
    oi <- 0
    for(i in test.doc){
      if ((substr(i[1],1,1) == "D")){
        di = di+1
      } else{
        if(((substr(i[1],1,1) != "D")))
          oi = oi+1
      }
    }
    
    noOfD2 <- noOfD-di
    noOfnD2 <- noOfnD-oi
    
    comp.List <- getDisTerm(A,dataset) # retrieve terms for each component
   
    dis.Comp <- getDisCompThres(train.set) # get discriminative components based on threshold
    
    docLst <- getDisCompComb(train.set,dis.Comp) # simple retrieval of comp-doc weights for discriminatory components 
    
    docTopics <- combineWeights(dataset,comp.List,docLst) # term-in-document weight combination
    
    test.terms <- t(t(A)%*%t(test.set)) # calculate test doc correspondence
    
    profiles <- getProfile(docTopics, 1.0) # get profiles for Dickens/ nonDickens, discard terms at mean by e.g. 2.5 
    D.profile <- as.matrix(profiles$D)
    nD.profile <- as.matrix(profiles$nD)
    D.orig[[l]] <- as.matrix(profiles$D.old)
    nD.orig[[l]] <- as.matrix(profiles$nD.old)
    
    
    D.feat[[l]] <- as.matrix(D.profile[order(D.profile[,1],decreasing = TRUE ),])
    O.feat[[l]] <- as.matrix(nD.profile[order(nD.profile[,1],decreasing = TRUE ),])
    
    
    inter.features <- intersect(rownames(D.profile),rownames(nD.profile))
    inter.feat[[l]] <- inter.features
    sim.DO <- dissim.Matrix(dataset,inter.features)
    cr <- getAdjRand(sim.DO, "complete", noOfD, noOfnD) # try with some other metric?
    sim[[l]]<- sim.DO
    clust.eval[[test.doc[1]]] <- cr
    
    for (n in test.doc){
      
      
      # test histogram differences
      D.diff[[n]] <- hist.diff(as.matrix(test.terms[n,]),D.profile)
      hist[[n]]  <-hist.diffC(as.matrix(test.terms[n,]),D.profile)  
      
      O.diff[[n]] <- hist.diff(as.matrix(test.terms[n,]),nD.profile)
      hist2[[n]]  <-hist.diffC(as.matrix(test.terms[n,]),nD.profile) 
      
    }
    
    i= i2+1
    i2 <- i+4
    
  }
  
  hist.results <- cv.results.3(D.diff,O.diff,clust.eval, hist, hist2) # change this to cv.3
  featConsist <- featureConsistency.2(D.feat)
  featConsist.2 <- featureConsistency.2(O.feat)
  
  
  cross.val <- list()
  cross.val[["D.diff"]] <- D.diff
  cross.val[["O.diff"]] <- O.diff
  cross.val[["hist.res"]] <- hist.results
  cross.val[["sim"]] <- sim
  cross.val[["feat"]] <- inter.feat
  cross.val[["D.feat"]] <- D.feat
  cross.val[["O.feat"]] <- O.feat
  cross.val[["D.consist"]] <- featConsist
  cross.val[["O.consist"]] <- featConsist.2
  cross.val[["D.feat.O"]] <- D.orig
  cross.val[["O.feat.O"]] <- nD.orig
  cross.val[["hist"]] <- hist
  cross.val[["hist2"]] <- hist2
  cross.val[["clust.eval"]] <- clust.eval 
    
    
    
    
    
  return(cross.val)
  
} 
    
    
  
  