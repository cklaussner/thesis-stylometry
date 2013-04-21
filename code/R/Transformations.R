
# Matrix Transformations

#-- change dtm term weighting with log.

Vsize <- dim(nV)
numOfDocs <- Vsize[1] 
numOfTerms <- Vsize[2] 

n.relFreq <- matrix(0,nrow=numOfDocs, numOfTerms)
rownames(n.relFreq) <- rownames(nV)
colnames(n.relFreq) <- colnames(nV)
for (n in rownames(nV)){
  
  curr.Doc <- nV[n,]
  w.Token <- sum(curr.Doc) # sum over all tokens in doc
  w.Type <- length(curr.Doc[curr.Doc != 0]) # not sure this is correct
  
  for(i in 1:length(curr.Doc)){
    
    curr.Vec <- curr.Doc[i]
    
    rel.Freq <- (curr.Vec +1)/ (w.Token+w.Type)
  
    n.relFreq[n,i] <- (rel.Freq)
  }
  
}
nV <- n.relFreq




m <- matrix(0, nrow = length(diff[[1]]),ncol=3)
rownames(m) <- names(diff[[1]])
colnames(m) <- c("Dist.D.","Dist.C.","(Collins-Dickens)")
for (n in names(diff[[1]])){
  
  d <- diff[[1]]
  c <- diff[[2]]
  val1 <- d[[n]]
  val2 <- c[[n]]
  m[n,1] <- val1
  m[n,2] <- val2
  m[n,3] <- val2-val1
}


m <- matrix(0, nrow = length(diff[[1]]),ncol=3)
rownames(m) <- names(diff[[1]])
colnames(m) <- c("Dist.D.","Dist.C.","(Dickens-Collins)")
for (n in names(diff[[1]])){
  
  d <- diff[[1]]
  c <- diff[[2]]
  val1 <- d[[n]]
  val2 <- c[[n]]
  m[n,1] <- val1
  m[n,2] <- val2
  m[n,3] <- val1-val2
}