
# Evaluation of Representativeness - Distinctiveness pure and simple 

evalRD1 <- function(dataset,noOfD,noOfO){

dsize <- size(dataset)
num.Docs <- dsize[1]  
num.Terms <- dsize[2]



for (i in 1:numDocs){
  
  print(i)
  test.set <- dataset[i,] # extract doc for test
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
  
  RD.features <- diff$features # get Dickens features
  dis.Matrix <- diff$dis.Matrix # get similarity matrix based on all terms individually
  
  # calculate histogram for RD features: Dickens
  termsize.D <- sum(RD.features)
  hist.D <- as.matrix(rep(0, length(RD.features)))
  rownames(hist.D) <- rownames(RD.features)
  for (n in rownames(RD.features)){
    hist.D[n,] <- RD.features[n,]/termsize.D
    }
  
  
    
    
    
    
    
  
}
  
}






