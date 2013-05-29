
# ICA functions


# select positive/negative terms for each component based on individual threshold

getDisTerm <- function(A,input){
  
colnames(A) <- colnames(input)  # do term names
rownames(A) <- c(1:numOfIC) # do comp names
compLst <- list()
  
numOfIC <- (dim(A))[1]

comp.thres <- as.matrix(rowMeans(abs(A)))
rownames(comp.thres) <- c(1:numOfIC)


for(i in 1:numOfIC){   # select keywords in components according to weight
  
  tmp <- A[i, ] #get all term weights for one component 
  
  terms.pos <- as.matrix(tmp[tmp> comp.thres[i]]) # only retain terms above component threshold
  
  terms.neg <- as.matrix(tmp[tmp < (-comp.thres[i]]) # only retain terms below - component threshold
  
  compLst[[i]] <-  rbind(terms.pos,terms.neg)     # add to overall comp list

}
 
return(compLst)
}

########### get component weights for each document, accounts for preselection of components for each group separately

getDisComp <- function(S,noOfD,noOfO,compForD,compFornD){
  
  
  prim.set <- S[1:noOfD, ]
  prim.set <- prim.set[,compForD] # reduce set
  sec.set <- S[(noOfD+1):(noOfD+noOfO), ]
  sec.set <- sec.set[,compFornD]
  docLst <- list()

for (n in rownames(prim.set)){ # for Dickens
  
  docLst[[n]]  <- as.matrix(prim.set[n,])    # save all retained comp in document matrix
}
 
for (n in rownames(sec.set)){ # for nonDickens
    
    docLst[[n]]  <- as.matrix(sec.set[n,])    # save all retained comp in document matrix
}
  
return(docLst)
  
}






