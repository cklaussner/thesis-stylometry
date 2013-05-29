
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
  
  terms.neg <- as.matrix(tmp[tmp < (-comp.thres[i])]) # only retain terms below - component threshold
  
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


#### combine term-component weight and component - document weight into term-component weight

combineWeights <- function(nV,compLst,docLst){
docTopics <- list()


for (n in names(docLst)){   #get relevant terms for each document
  
  keyList <- as.matrix(rep(0, (dim(nV)[2]))) # overall list for keywords for document - begin with entry for all terms possible
  rownames(keyList) <- colnames(nV)
  
  compList <- docLst[[n]] # get component list for doc
  
  for (c in rownames(compList)){ 
    
    compWeight <- as.double(compList[c,]) # get component weight 
   
    keys <- compLst[[as.integer(c)]] # get terms for comp. 
    
    if ((length(keys))!=0){
      
      keyWeight <- keys*compWeight # raise all keys in comp by weight it has in document #TODO - have 2* compWeight???
      
      keyList[rownames(keyWeight),] <- keyList[rownames(keyWeight),] + keyWeight[,1] # add to previous weights for keywords
      }
    
  }
  names(keyList) <- rownames(keyList)
  keyList <- as.matrix(keyList[keyList !=0])
  docTopics[[n]] <- as.matrix(keyList[order(keyList[,1],decreasing = TRUE ),])
}

return(docTopics)
}

# choose author terms - average of term over all of authors documents












