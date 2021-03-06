# evalRD1-5 cross-validation

#read.csv('world.csv', row.names=1)
source("RepDisPur.R")
source("functions.R")
# Evaluation of Representativeness - Distinctiveness pure and simple 

#source("evalRD15.R")

#diff <- evalRD15(nV,45,29,1500, 1.1)
#diff <- evalRD15(nV,45,29,1500, 1.1)

diff <- evalRD15(wM,24,55,1431, 1)


evalRD15 <- function(dataset,noOfD,noOfO, noInputFeat, alpha){
  
  
  dsize <- dim(dataset)  # set parameters
  num.Docs <- dsize[1]  
  num.Terms <- dsize[2]
  
  D.diff <- list() # initialise output lists
  
  O.diff <- list()

  sim <- list()
  iter.list <- list()
  
  D.feat <- list()
  O.feat <- list()
  D.feat.p <- list()
  O.feat.p <- list()
  clust.eval <- list()
  cross.val <- list()
  hist <- list()
  hist2 <- list()
  RDFeat.orig.1 <- list()
  RDFeat.orig.2 <- list()
  inter.feat.col <- list()
  
  dataset <- as.matrix(check.data(dataset,noOfD,noOfO)) # check that Dickens is first in set
  
  term.dist <- distribute.RD(dataset, noOfD, noOfO) # check freq. dist. of terms for both sets
  d.terms <- term.dist$D
  nd.terms <- term.dist$nD
  
  iterations <- as.integer(num.Docs/5)
  i <- 1
  i2<- 1+4
  for (l in 1:1){
    
    print(l)  # current interation
  
    test.set <- as.matrix(dataset[i:i2,]) # extract doc for test
    test.doc <- rownames(dataset)[i:i2]
    train.set <- dataset[!rownames(dataset) %in% test.doc, ] # create new matrix with document left out
    iter.list[[l]] <- test.doc
    #--- this is to know of what author there has been a reduction  -- should be an easier way
    
    
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
    
      numOfD <- noOfD-di
      numOfnD <- noOfO-oi
    
    #diff <- repDis(log(train.set),numOfD,numOfnD,noInputFeat, alpha)  # do R. and D. selection for curr. training set 
    diff <- repDis(train.set,numOfD,numOfnD,noInputFeat, alpha) 
   
    RDFeat.orig.1[[l]] <- diff$features.orig
    RDFeat.orig.2[[l]] <- diff$features.orig.2
    
    RD.old  <- diff$features.1 # get Dickens features and reduce 
    D.feat.p[[l]] <- as.matrix(RD.old[order(RD.old[,1],decreasing = TRUE ),]) # save the orinal set
    rd1 <- intersect(rownames(RD.old),d.terms)
    RD.features <- as.matrix(RD.old[rd1,])
    RD.features <- as.matrix(RD.features[order(RD.features[,1],decreasing = TRUE ),])
    D.feat[[l]] <- RD.features
    
    RD2.old <-diff$features.2 # get other features
    O.feat.p[[l]] <- as.matrix(RD2.old[order(RD2.old[,1],decreasing = TRUE ),])  # save the orinal set
    rd2 <- intersect(rownames(RD2.old),nd.terms)
    RD.features.2 <- as.matrix(RD2.old[rd2,])
    RD.features.2 <- as.matrix(RD.features.2[order(RD.features.2[,1],decreasing = TRUE ),])
    O.feat[[l]] <- RD.features.2
    
    
    
    # calculate histogram diff. for RD features: Dickens
    
    for (n in test.doc){
    
    D.diff[[n]] <- hist.diff(as.matrix(test.set[n,]),RD.features)
    hist[[n]]  <-hist.diffC(as.matrix(test.set[n,]),RD.features)  # intersection most frequent + normal list each
    
    O.diff[[n]] <- hist.diff(as.matrix(test.set[n,]),RD.features.2)
    hist2[[n]]  <-hist.diffC(as.matrix(test.set[n,]),RD.features.2)
    }
    # calculate histogram diff. for RD features: Collins/other set
    
    inter.feat <- intersect(rownames(RD.old), rownames(RD2.old)) # get features in both sets
    sim.DO <- dissim.Matrix(dataset,inter.feat) # calculate similarity matrix based on RD.features: Dickens
    cr <- getAdjRand(sim.DO, "complete", noOfD, noOfO) # try with some other metric?
    sim[[l]]<- sim.DO
    clust.eval[[test.doc[1]]] <- cr
    inter.feat.col[[l]] <- inter.feat
    
   i= i2+1
   i2 <- i+4
    }
  
  #----------- sum up results of cross-validation
  
  #hist.results <- cv.results.3(D.diff,O.diff,clust.eval, hist, hist2)
  
  #featConsist <- featureConsistency.2(D.feat.p)
  #featConsist.2 <- featureConsistency.2(O.feat.p)
  
  #cross.val[["hist.res"]] <- hist.results
  cross.val[["cv"]] <- clust.eval
  cross.val[["sim"]] <- sim
  cross.val[["inter.feat"]] <- inter.feat.col
  cross.val[["D.feat"]] <- D.feat
  cross.val[["O.feat"]] <- O.feat
  cross.val[["D.feat.O"]] <- D.feat.p
  cross.val[["O.feat.O"]] <- O.feat.p
  cross.val[["D.diff"]] <- D.diff
  cross.val[["O.diff"]] <- O.diff
  #cross.val[["D.consist"]] <- featConsist
  #cross.val[["O.consist"]] <- featConsist.2
  cross.val[["hist"]] <- hist
  cross.val[["hist2"]] <- hist2
  cross.val[["RD.orig.1"]] <-  RDFeat.orig.1
  cross.val[["RD.orig.2"]] <-  RDFeat.orig.2
  
  
  
  
  
  return(cross.val)
  
}





