library(fastICA)

# compute ICA on the basis of input matrix and extract terms according to component weight in document/term weight in component 


ICA1 <- function(nV,numOfIC){
  
  Vsize <- dim(nV)
  numOfTestDocs <- Vsize[1] 
  numOfTerms <- Vsize[2] 
  
  Y<- fastICA(nV, numOfIC, alg.typ = "deflation",
              fun = "exp", alpha = 1.0, method = "R",
              row.norm = TRUE, maxit = 200, tol = 1e-04, verbose = TRUE,
              w.init = NULL)
  
  
  S <- Y$S # extract doc-by-comp 
  A <- (Y$A) # get inverse of unmixing matrix W
  
  Asize <- dim(A)
  numOfIC <- Asize[1] 
  numOfTerms <- Asize[2] 
  
  Ssize <- dim(S)
  numOfDocs <- Ssize[1]
  
  compT <- A 
  docs <- S 
  
  colnames(docs) <- paste(1:numOfIC)   # name components
  docnames <- rownames(nV)
  
  colnames(docs) <- paste(1:numOfIC)   # name components
  docnames <- rownames(nV)
  
  colnames(compT) <- colnames(nV)  # get term names
  compLst <- list()
  compLst.neg <- list()
  
  #-------------------------create list where for each comp - list of keyword-weight pair 
  
  
  # !!!TODO - change term threshold according to new scheme
  compThres <- list()
  
  for(i in 1:numOfIC) {   # sort keywords in components according to weight
    
     compThres[[i]] <- sum(abs(compT[i, ]))/numOfDocs   #test this!!!
    
     tmp <- sort(compT[i, ],decreasing = TRUE) # sort comp. according to most prominent keywords
    
    pos.key <-tmp[tmp > keyThres]
    terms.pos <- as.matrix(pos.key)
    rownames(terms.pos) <- names(pos.key)
    
    neg.key <- sort(tmp[tmp < 0.0],decreasing = FALSE) # still have to change this to key threshold
    terms.neg <- as.matrix(neg.key)
    rownames(terms.neg) <- names(neg.key)
    
    compLst[[i]] <-  terms.pos     # add to overall comp list
    
    compLst.neg[[i]]  <- terms.neg
    
  }
  
  
  for (i in 1:length(compLst)){
    if (length(compLst[[i]])==0){
      print(i)
      print("Warning - no keywords for comp")}}
  
  #---------------------------# same for components for each doc - order according to weight - pos in list = pos in doc
  
  docLst <- list()
  docLst.neg <- list()
  
  for (i in 1:numOfDocs){   # order comp for each document
    
    
    comp.ord <- sort(docs[i, ],decreasing = TRUE)  # sort accord. to weight
    
    pos.comp <- comp.ord[comp.ord > compThres] # here is the problem
    
    
    comp.pos <- as.matrix(pos.comp)
    rownames(comp.pos) <- names(pos.comp)
    
    neg.comp <- sort(comp.ord[comp.ord < 0.0],decreasing = FALSE)
    comp.neg <- as.matrix(neg.comp)
    rownames(comp.neg) <- names(neg.comp)
    
    docLst[[docnames[i]]] <-  comp.pos   # add to overall comp list
    
    docLst.neg[[docnames[i]]]  <- comp.neg
  }
  
  for (i in 1:length(docLst)){
    
    if (length(docLst[[i]])==0){
      print(i)
      print("Warning - no comp for doc")
    }}
  
  
  
  #----------------------------------
  docTopics <- list()
  
  for (n in names(docLst)){   # get keywords for each document
    
    complist <- docLst[[n]] # get component list for each doc
    
    keylist <- as.matrix(rep(0, numOfTerms)) # overall list for keywords for document - begin with entry for all terms possible
    rownames(keylist) <- colnames(nV)
    
    for (c in rownames(complist)){ # for all comp for doc
      
      compweight <- as.double(complist[c,])
      
      keys <- compLst[[as.integer(c)]] # get keywords for comp. 
      
      if ((length(keys))!=0){
        
        keyweight <- keys*(2*compweight) # raise all keys in comp by weight it has in document
        
        
        keylist[rownames(keyweight),] <- keylist[rownames(keyweight),] + keyweight[,1] # add to previous weights for keywords
        
      }
      
    }
    names(keylist) <- rownames(keylist)
    keylist <- as.matrix(keylist[keylist >0])
    keylist <- keylist[order(keylist[,1],decreasing = TRUE ),]
    docTopics[[n]] <- as.matrix(keylist)
    
  } 
  
  
  
  
  
  
  
  
  
  
} 