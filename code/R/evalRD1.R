source("RepDisPur.R")
library(psych)
# Evaluation of Representativeness - Distinctiveness pure and simple 

evalRD1 <- function(dataset,noOfD,noOfO){
  
  # check frequencies of terms
  if ((substr(rownames(dataset)[1],1,1) == "D")){
    print("Dickens is first")
    prim.set <- dataset[1:noOfD, ]
    sec.set <- dataset[(noOfD+1):(noOfD+noOfO), ]
  }else{
    print("Compare set is first")
    sec.set <- dataset[1:noOfO, ]
    prim.set <- dataset[(noOfO+1):(noOfD+noOf0), ]
  }
  
  freq.list <- matrix(0,nrow= length(colnames(dataset)), ncol=2)
  rownames(freq.list) <- colnames(dataset)
  colnames(freq.list) <- c("D","nD")
  d.terms <- c()
  nd.terms <- c()
  for (l in colnames(dataset)){
    
    D <- sum(prim.set[,l])/noOfD
    nD <- sum(sec.set[,l])/noOfO
    if (D > nD){
    freq.list[l,1] <- sum(prim.set[,l])/noOfD
    d.terms <- c(d.terms,l)
    }else{
      freq.list[l,2] <- sum(sec.set[,l])/noOfO
      nd.terms <- c(nd.terms,l)
    }
  }
  
  
  #--------------------
  
  D.diff <- list()
  O.diff <- list()
  D.sim <- list()
  O.sim <- list()
  D.feat <- list()
  O.feat <- list()
  cross.val <- list()
  

dsize <- dim(dataset)
num.Docs <- dsize[1]  
num.Terms <- dsize[2]

for (i in 1:2){
  
  print(i)
  test.set <- as.matrix(dataset[i,]) # extract doc for test
  remove.doc <- rownames(dataset)[i]
  train.set <- dataset[!rownames(dataset) %in% remove.doc, ] # create new matrix with document left out
  
  #--- this is to know of what author there has been a reduction  -- should be an easier way
  num.of.D <- noOfD
  num.of.nD <- noOfO
  
  if (i <= noOfD){
    num.of.D <- num.of.D-1
    }else{
      num.of.nD <- num.of.nD-1
    }
  #-----
  
  diff <- repDis(train.set,num.of.D,num.of.nD,1.1)
  
  RD.features <- diff$features.1 # get Dickens features
  rd1 <- intersect(rownames(RD.features),d.terms)
  RD.features <- as.matrix(RD.features[rd1,])
  D.feat[[remove.doc]] <- RD.features
  
  RD.features.2 <-diff$features.2 # get other features
  rd2 <- intersect(rownames(RD.features.2),nd.terms)
  RD.features.2 <- as.matrix(RD.features.2[rd2,])
  O.feat[[remove.doc]] <- RD.features.2
  
  dis.Matrix <- diff$dis.Matrix # get similarity matrix based on all terms individually
  dis.Matrix.2 <- diff$dis.Matrix.2 # get similarity matrix for other authors
  
  # calculate histogram for RD features: Dickens
  hist.D <- as.matrix(rep(0, length(RD.features)))
  termsize.D <- sum(RD.features)
  hist.D <- RD.features/termsize.D
    
  # extract Dickens keywords from test set vector
  test.vec <- as.matrix(rep(0, length(RD.features)))
  rownames(test.vec) <- rownames(RD.features)
  test.vec <- as.matrix(test.set[rownames(RD.features),]) # retain only RD.features 
  termsize.T <- sum(test.vec)
  
  # calculate histogram for RD features: test doc
  hist.test <- as.matrix(rep(0, length(RD.features)))
  rownames(hist.test) <- rownames(RD.features)
  hist.test <- test.vec/termsize.T
  
  abs.diff <- (sum(abs(hist.D - hist.test)))/length(RD.features) # calculate absolute difference
  D.diff[[remove.doc]] <- abs.diff
  
  
  # calculate histogram for RD features: Collins/other set
  hist.O <- as.matrix(rep(0, length(RD.features.2)))
  termsize.D <- sum(RD.features.2)
  hist.O <- RD.features.2/termsize.D
  
  # extract Collins/other set keywords from test set vector
  test.vec.2 <- as.matrix(rep(0, length(RD.features.2)))
  rownames(test.vec.2) <- rownames(RD.features.2)
  test.vec.2 <- as.matrix(test.set[rownames(RD.features.2),]) # retain only RD.features 
  termsize.T2 <- sum(test.vec)
  
  # calculate histogram for RD features 2: test doc
  hist.test.2 <- as.matrix(rep(0, length(RD.features.2)))
  rownames(hist.test.2) <- rownames(RD.features.2)
  hist.test.2 <- test.vec.2/termsize.T2
  
  abs.diff.2 <- (sum(abs(hist.O - hist.test.2)))#/length(RD.features.2) # calculate absolute difference
  O.diff[[remove.doc]] <- abs.diff.2
  


  #---- calculate similarity matrix based on RD.features: Dickens
  
  matrix.RD <- as.matrix(dis.Matrix[[1]])
  if (length(dis.Matrix) > 1){
  for (i in 2:length(dis.Matrix)){
  
    m <- as.matrix(dis.Matrix[[i]])
    matrix.RD <- (matrix.RD + m)
    
  }
  }
  D.sim[[remove.doc]] <- matrix.RD/length(dis.Matrix)
   
  
  #----calculate similarity matrix based on RD.features: Other
  
  
  matrix.RD2 <- as.matrix(dis.Matrix.2[[1]])
  if (length(dis.Matrix.2) > 1){
  for (i in 2:length(dis.Matrix.2)){
  
    m <- as.matrix(dis.Matrix.2[[i]])
    matrix.RD2 <- matrix.RD2 +m
    
  }
  }
  O.sim[[remove.doc]] <- matrix.RD2/length(dis.Matrix.2)
  
}

  
  #----------- sum up results of cross-validation
  
  dickens.list <- names(D.diff)[substr((names(D.diff)),1,1) == "D"]
  print(dickens.list)
  other.list <- names(O.diff)[substr((names(O.diff)),1,1) != "D"]
  
  dsize <- length(dickens.list)
  osize <- length(other.list)
  results <- matrix(0, nrow = (dsize+2),ncol=3)
  rownames(results) <- c(dickens.list,"mean","sum")
  colnames(results) <- c("Dist.D.","Dist.C.","(Collins-Dickens)")
  d <- D.diff
  c <- O.diff
  
  for (n in dickens.list){
    results[n,1] <- d[[n]]
    results[n,2] <- c[[n]]
    results[n,3] <- c[[n]]-d[[n]]
    
  }
  results[dsize+1,] <- c(mean(results[1:dsize,1]),mean(results[1:dsize,2]),mean(results[1:dsize,3]))
  results[dsize+2,] <- c(sum(results[1:dsize,1]),sum(results[1:dsize,2]),sum(results[1:dsize,3]))
  
  results.2 <- matrix(0, nrow = (osize+2),ncol=3)
  rownames(results.2) <- c(other.list,"mean","sum")
  colnames(results.2) <- c("Dist.D.","Dist.C.","(Dickens-Collins)")
  d <- D.diff
  c <- O.diff
  
  for (n in other.list){
    
    results.2[n,1] <- d[[n]]
    results.2[n,2] <- c[[n]]
    results.2[n,3] <- d[[n]]-c[[n]]
    
  }
  results.2[osize+1,] <- c(mean(results.2[1:osize,1]),mean(results.2[1:osize,2]),mean(results.2[1:osize,3]))
  results.2[osize+2,] <- c(sum(results.2[1:osize,1]),sum(results.2[1:osize,2]),sum(results.2[1:osize,3]))
  
  
cross.val[["Dickens"]] <- results
cross.val[["Other"]] <- results.2
cross.val[["D.sim"]] <- D.sim
cross.val[["O.sim"]] <- O.sim
cross.val[["D.feat"]] <- D.feat
cross.val[["O.feat"]] <- O.feat
cross.val[["features"]] <- diff
  
  
  
return(cross.val)
  
}






