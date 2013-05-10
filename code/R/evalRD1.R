
# Evaluation of Representativeness - Distinctiveness pure and simple 

evalRD1 <- function(dataset,noOfD,noOfO){
  
  D.diff <- list()
  O.diff <- list()
  

dsize <- size(dataset)
num.Docs <- dsize[1]  
num.Terms <- dsize[2]

for (i in 1:numDocs){
  
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
  #---
  
  diff <- repDis(train.set,num.of.D,num.of.nD,setToTest)
  
  RD.features <- diff$features.1 # get Dickens features
  RD.features.2 <-diff$features.2 # get other features
  dis.Matrix <- diff$dis.Matrix # get similarity matrix based on all terms individually
  
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
  
  abs.diff <- sum(abs(hist.D - hist.test)) # calculate absolute difference
 
    
    
  D.diff[[remove.doc]] <- abs.diff
  
  O.diff[[remove.doc]] <- abs.diff2
  
    
    
  
}

return()
  
}






