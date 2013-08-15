
# ICA functions
library(psych)

# select positive/negative terms for each component based on individual threshold

getDisTerm <- function(A,input){
  
numOfIC <- (dim(A))[1] 
colnames(A) <- colnames(input)  # do term names
rownames(A) <- c(1:numOfIC) # do comp names
compLst <- list()
  

term.thres <- as.matrix(colMeans(abs(A)))
#term.thres.SD <- as.matrix(SD(abs(A)))   # compute std for each term 
#term.thres <- term.thres + term.thres.SD # add to threshold, so if values fluctuate more, this is reflected 


for(i in 1:numOfIC){   # select keywords in components according to weight
  
  tmp <- as.matrix(A[i, ]) #get all term weights for one component 
  names(tmp) <- rownames(tmp)
  
  terms.pos <- as.matrix(tmp[tmp> term.thres])# only retain terms above term threshold
  #terms.pos <- as.matrix(terms.pos[order(terms.pos[,1],decreasing = TRUE ),])
  
  terms.neg <- as.matrix(tmp[tmp < (-term.thres)]) # only retain terms below - term threshold - so high negative association
  
  #terms.pos <- as.matrix(tmp) # only retain terms above term threshold
  
  #terms.neg <- as.matrix(tmp) # only retain terms below - term threshold - so high negative association
  
  terms.all <- rbind(terms.pos,terms.neg)
  terms.all <- as.matrix(terms.all[order(terms.all[,1],decreasing = TRUE ),])
  
  
  compLst[[i]] <-  terms.all     # add to overall comp list

}
 
return(compLst)
}


# simple unsupervised: component for document selection based on individual component thresholds
getDisCompThres <- function(S){
  
  numOfDocs <- (dim(S))[1] 
  numOfIC <- (dim(S))[2] 
  #rownames(S) <- rownames(input)  # do doc names
  #colnames(S) <- c(1:numOfIC) # do comp names
  comp.thres <- as.matrix(colMeans(abs(S)))
  #comp.thres.SD <- as.matrix(SD(abs(S)))
  #comp.thres <- as.matrix(comp.thres+comp.thres.SD) # have higher threshold for components 
  compLst <- list()
 
  for(i in rownames(S)){
    
    tmp <- as.matrix(S[i, ]) #get all comp weights for one doc
    
    names(tmp) <- rownames(tmp)
    
    comp.pos <- rownames(as.matrix(tmp[tmp> comp.thres])) # only retain terms above term threshold
    
    comp.neg <- rownames(as.matrix(tmp[tmp < (-comp.thres)])) # only retain terms below - term threshold - so high negative association
    
    #comp.pos <- rownames(as.matrix(tmp))
    #comp.neg <- rownames(as.matrix(tmp))
    
    compLst[[i]] <-  c(comp.pos,comp.neg)     # add to overall comp list
    
  }
return(compLst)
}


########### get component weights for each document based on preselection of components for each group separately- practically this is not doing much

getDisCompComb <- function(S,compList){
  
  docLst <- list()

for (n in rownames(S)){ # for Dickens & nonDickens
  
  retain.comp <- compList[[n]]
  docLst[[n]]  <- as.matrix(S[n,c(retain.comp)])    # save all retained comp in document matrix
}
 

  
return(docLst)
  
  
  
}

getDisCompRD <- function(S,noOfD,noOfO,compForD,compFornD){
  
  
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
      
      keyWeight <- keys*compWeight # raise all keys in comp by weight it has in document
      
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


getProfile <- function(docTopics, alpha){
  profiles <- list()

  D.termlist <- list()
  nD.termlist <- list()
  
  dickens.list <- names(docTopics)[substr((names(docTopics)),1,1) == "D"] # get Dickens elements tested 
  nonDickens.list <- names(docTopics)[substr((names(docTopics)),1,1) != "D"] # get nonDickens elements tested 
  
  for (n in dickens.list){
    #Dickens profile
    
    for (t in rownames(docTopics[[n]])){
      
      if (t %in% names(D.termlist)){
        D.termlist[[t]] <- c(D.termlist[[t]], docTopics[[n]][t,])
        
      }else{
        
        D.termlist[[t]] <- docTopics[[n]][t,]
        
      }
      }
    
    }
  
  D.profile <- as.matrix(rep(0,length(D.termlist)))
  rownames(D.profile)<- names(D.termlist)                      
  for (t in names(D.termlist)){
    D.profile[t,] <- mean(D.termlist[[t]])
  }
  names(D.profile)<- rownames(D.profile)
  profiles[["D.old"]] <- D.profile
  D.sd <- sd(abs(D.profile)) 
  D.profile <- D.profile[abs(D.profile)> (((mean(abs(D.profile)))+D.sd)*alpha)]
  #D.profile <- D.profile[abs(D.profile)> (((mean(abs(D.profile)))*alpha)+D.sd)]
  
  ###################beginning of nonDickens profile selection
  for (n in nonDickens.list){
    #nonDickens profile
    
    for (t in rownames(docTopics[[n]])){
      
      if (t %in% names(nD.termlist)){
        nD.termlist[[t]] <- c(nD.termlist[[t]], docTopics[[n]][t,])
        
      }else{
        
        nD.termlist[[t]] <- docTopics[[n]][t,]
        
      }
    }
    
  }
  
  nD.profile <- as.matrix(rep(0,length(nD.termlist)))
  rownames(nD.profile)<- names(nD.termlist)                      
  for (t in names(nD.termlist)){
    nD.profile[t,] <- mean(nD.termlist[[t]])
  }
  names(nD.profile)<- rownames(nD.profile)
  profiles[["nD.old"]] <- nD.profile
  nD.sd <- sd(abs(nD.profile)) 
  nD.profile <- nD.profile[abs(nD.profile)> (((mean(abs(nD.profile)))+nD.sd)*alpha)]
  #nD.profile <- nD.profile[abs(nD.profile)> (((mean(abs(nD.profile)))*alpha)+nD.sd)]
  
 
profiles[["D"]] <- D.profile
profiles[["nD"]] <- nD.profile
return(profiles)
}









