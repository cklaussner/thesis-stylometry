source("RepDisPur.R")
source("functions.R")
# Evaluation of Representativeness - Distinctiveness pure and simple 

evalRD1 <- function(dataset,noOfD,noOfO, noInputFeat, alpha){
  
  
  dsize <- dim(dataset)  # set parameters
  num.Docs <- dsize[1]  
  num.Terms <- dsize[2]
  
  D.diff <- list() # initialise output lists
  O.diff <- list()
  D.sim <- list()
  O.sim <- list()
  D.feat <- list()
  O.feat <- list()
  D.feat.p <- list()
  O.feat.p <- list()
  clust.eval <- list()
  clust.eval.2 <- list()
  cross.val <- list()
  
  dataset <- as.matrix(check.data(dataset,noOfD,noOfO)) # check that Dickens is first in set
  term.dist <- distribute.RD(dataset, noOfD, noOfO) # check freq. dist. of terms for both sets
  n.terms <- term.dist$D
  nd.terms <- term.dist$nD 
  
for (i in 1:2){
  
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
  
  RD.features <- diff$features.1 # get Dickens features and reduce 
  D.feat.p[[test.doc]] <- RD.features  # save the orinal set
  rd1 <- intersect(rownames(RD.features),d.terms)
  RD.features <- as.matrix(RD.features[rd1,])
  D.feat[[test.doc]] <- RD.features
  
  RD.features.2 <-diff$features.2 # get other features
  O.feat.p[[test.doc]] <- RD.features.2  # save the orinal set
  rd2 <- intersect(rownames(RD.features.2),nd.terms)
  RD.features.2 <- as.matrix(RD.features.2[rd2,])
  O.feat[[test.doc]] <- RD.features.2
  
  
  
  # calculate histogram diff. for RD features: Dickens
  
  D.diff[[test.doc]] <- hist.diff(test.set,RD.features)
  
  
  # calculate histogram diff. for RD features: Collins/other set
  O.diff[[test.doc]] <- hist.diff(test.set,RD.features.2)

  
 

   sim.D <- dissim.Matrix(dataset,RD.features) # calculate similarity matrix based on RD.features: Dickens
   cr <- getCorrRand(sim.D, "complete", num.of.D, num.of.nD) # try with some other metric?
   D.sim[[test.doc]]<- sim.D
   clust.eval[[test.doc]] <- cr
   
   sim.O <- dissim.Matrix(dataset, RD.features.2) #calculate similarity matrix based on RD.features: Other
   cr.2 <- getCorrRand(sim.0, "complete", num.of.D, num.of.nD) # try with some other metric?
   O.sim[[test.doc]] <- sim.O
   clust.eval.2[[test.doc]] <- cr.2


}
 #----------- sum up results of cross-validation
  
hist.results <- cv.results(D.diff,O.diff,clust.eval,clust.eval.2)
  
  
cross.val[["hist.res"]] <- hist.results
cross.val[["D.sim"]] <- D.sim
cross.val[["O.sim"]] <- O.sim
cross.val[["D.feat"]] <- D.feat
cross.val[["O.feat"]] <- O.feat
cross.val[["D.feat.O"]] <- D.feat.p
cross.val[["O.feat.O"]] <- O.feat.p
  
  
return(cross.val)
  
}






