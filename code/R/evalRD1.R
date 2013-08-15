source("RepDisPur.R")
source("functions.R")
# Evaluation of Representativeness - Distinctiveness pure and simple 

#source("evalRD1.R")

#diff <- evalRD1(nV,55,31,20, 1.1)


evalRD1 <- function(dataset,noOfD,noOfO, noInputFeat, alpha){
  
  
  dsize <- dim(dataset)  # set parameters
  num.Docs <- dsize[1]  
  num.Terms <- dsize[2]
  
  D.diff <- list() # initialise output lists
  O.diff <- list()
  sim <- list()
  
  D.feat <- list()
  O.feat <- list()
  D.feat.p <- list()
  O.feat.p <- list()
  clust.eval <- list()
  cross.val <- list()
  hist <- list()
  hist2 <- list()
  
  dataset <- as.matrix(check.data(dataset,noOfD,noOfO)) # check that Dickens is first in set
  
  term.dist <- distribute.RD(dataset, noOfD, noOfO) # check freq. dist. of terms for both sets
  d.terms <- term.dist$D
  nd.terms <- term.dist$nD
  
for (i in 1:1){
  
  print(i)  # current interation
  
  
  test.set <- as.matrix(dataset[i,]) # extract doc for test
  test.doc <- rownames(dataset)[i]
  train.set <- dataset[!rownames(dataset) %in% test.doc, ] # create new matrix with document left out
  
  #--- this is to know of what author there has been a reduction  -- should be an easier way
  num.of.D <- noOfD
  num.of.nD <- noOfO
  
  if (i <= noOfD){
    num.of.D <- num.of.D-1
    }else{
      num.of.nD <- num.of.nD-1
    }
  
  
  diff <- repDis(train.set,num.of.D,num.of.nD,noInputFeat, alpha)  # do R. and D. selection for curr. training set 
  
  
  RD.old  <- diff$features.1 # get Dickens features and reduce 
  D.feat.p[[test.doc]] <- as.matrix(RD.old[order(RD.old[,1],decreasing = TRUE ),]) # save the orinal set
  rd1 <- intersect(rownames(RD.old),d.terms)
  RD.features <- as.matrix(RD.old[rd1,])
  RD.features <- as.matrix(RD.features[order(RD.features[,1],decreasing = TRUE ),])
  D.feat[[test.doc]] <- RD.features
  
  RD2.old <-diff$features.2 # get other features
  O.feat.p[[test.doc]] <- as.matrix(RD2.old[order(RD2.old[,1],decreasing = TRUE ),])  # save the orinal set
  rd2 <- intersect(rownames(RD2.old),nd.terms)
  RD.features.2 <- as.matrix(RD2.old[rd2,])
  RD.features.2 <- as.matrix(RD.features.2[order(RD.features.2[,1],decreasing = TRUE ),])
  O.feat[[test.doc]] <- RD.features.2
  
  
  
  # calculate histogram diff. for RD features: Dickens
  
  
  D.diff[[test.doc]] <- hist.diff(test.set,RD.features)
  hist[[test.doc]]  <-hist.diffC(test.set,RD.features)  # intersection most frequent + normal list each
  
  # calculate histogram diff. for RD features: Collins/other set
   O.diff[[test.doc]] <- hist.diff(test.set,RD.features.2)
   hist2[[test.doc]]  <-hist.diffC(test.set,RD.features.2)
  
   inter.feat <- intersect(rownames(RD.old), rownames(RD2.old)) # get features in both sets
   sim.DO <- dissim.Matrix(dataset,inter.feat) # calculate similarity matrix based on RD.features: Dickens
   cr <- getAdjRand(sim.DO, "complete", noOfD, noOfO) # try with some other metric?
   sim[[test.doc]]<- sim.DO
   clust.eval[[test.doc]] <- cr
   
  
 }
  
 #----------- sum up results of cross-validation
  
  hist.results <- cv.results(D.diff,O.diff,clust.eval,hist,hist2)
 
  featConsist <- featureConsistency(D.feat)
  featConsist.2 <- featureConsistency(O.feat)
  
  
cross.val[["hist.res"]] <- hist.results
cross.val[["D.diff"]] <- D.diff
cross.val[["O.diff"]] <- O.diff
  
cross.val[["clust.eval"]] <- clust.eval
cross.val[["sim"]] <- sim
cross.val[["feat"]] <- inter.feat
cross.val[["D.feat"]] <- D.feat
cross.val[["O.feat"]] <- O.feat
cross.val[["D.feat.O"]] <- D.feat.p
cross.val[["O.feat.O"]] <- O.feat.p
cross.val[["D.consist"]] <- featConsist
cross.val[["O.consist"]] <- featConsist.2
cross.val[["hist"]] <- hist
cross.val[["hist2"]] <- hist2

return(cross.val)
  
}






