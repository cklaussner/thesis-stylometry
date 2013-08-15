
extractKeys <- function(nV,S, A, keyThres, compThres){ ###TODO change nV naming
  
  Asize <- dim(A)
  numOfComps <- Asize[1] 
  numOfTerms <- Asize[2] 

  Ssize <- dim(S)
  numOfDocs <- Ssize[1] 
  numOfComps <- Ssize[2] 

  compT <- abs(A) # get absolute vals since ica model is ambiguous in regard to sign 
  docs <- abs(S)

  colnames(docs) <- paste(1:numOfComps)   # name components
  docnames <- rownames(nV)

  colnames(compT) <- colnames(nV)  # get term names
  compLst <- list()
  
#-------------------------create list where for each comp - list of keyword-weight pair 
  
  for(i in 1:numOfComps) {   # sort keywords in components according to weight
    key <- sort(compT[i, ],decreasing = TRUE) # sort comp. according to most prominent keywords
    label <- names(key)  # get term names
    names(key) <- NULL # rmv term names
    lst <- list()
    for (j in 1:numOfTerms){ # for comp create list key - weight for easy access 
      if(key[j] < keyThres){
        break
      }
      
      lst[[j]] <- cbind(label[j],key[j])
    }
    
    compLst[[i]] <-  as.list(lst)     # add to overall comp list
    
  }
#---------------------------# same for components for each doc - order according to weight - pos in list = pos in doc

 docLst <- list()
 for (i in 1:numOfDocs){   # order comp for each document
  
    c_ord <- sort(docs[i, ],decreasing = TRUE)  # sort accord. to weight
  
    label <- names(c_ord) # get comp names
    names(c_ord) <- NULL # rmv comp names
    lst <- list()
  
    for (j in 1:numOfComps){ 
    
      if(c_ord[j] < compThres){
        break
    }
      lst[[j]] <- cbind(label[j],c_ord[j])
  }
  
  docLst[[docnames[i]]] <-  as.list(lst)    # add to overall comp list
}



return(docTopics)
}

writeKeys <- function(doc.topics){
  
  sink("KeyCol.txt")
  for(s in 1:length(doc.topics)){
    labels <- names(doc.topics)
    d <- doc.topics[[s]]
    ds <- paste(names(d),collapse=", ")
    cat("\n")
    cat(labels[s])
    cat("\n")
    cat(ds)
  }
  sink()
}


 

collectMaxTerms <- function(docTopics,maxTerms){ # ----------------------- get overall word frequencies
docLength <- length(docTopics) # no. of Docs
terms <- list() # list initialisation
for (i in 1:docLength){
  terms[[names(docTopics[i])]]<- rownames(docTopics[[i]])  # collect all terms for each doc
}

all.terms <- c()
for (d in names(terms)) { all.terms <- union(all.terms, terms[[d]])}   # take union of all terms in set


terms.count <- as.matrix(rep(0, length(all.terms)))
terms.countS <- as.matrix(rep(0, length(all.terms)))
rownames(terms.count) <- all.terms
rownames(terms.countS) <- all.terms
for (d in names(terms)) {
  if (substr(d,1,1) == "D"){
    d.count <- as.matrix(xtabs(~terms[[d]]))
    terms.count[rownames(d.count),] <- terms.count[rownames(d.count),] + d.count[,1]
    
  }else{ if(substr(d,1,1) == "W"){
    dS.count <- as.matrix(xtabs(~terms[[d]]))
    terms.countS[rownames(dS.count),] <- terms.countS[rownames(dS.count),] + dS.count[,1]
    
    
  }
  }}

terms.order <- terms.count[order(terms.count[,1],decreasing = TRUE ),]    # order doc sets according to freq.
termsS.order  <- terms.countS[order(terms.countS[,1],decreasing = TRUE ),]

print(termsS.order)

finalKeysD <- as.matrix(terms.order)[1:maxTerms,1]
finalKeysC <- as.matrix(termsS.order)[1:maxTerms,1]

sink("KeyCol.txt")
  
labelsD <- names(finalKeysD)

labelsC <- names(finalKeysC)

  cat("\n")
  cat("Dickens'Keywords")
  cat("\n")
  cat(labelsD)
  cat("\n")
  cat("Collins'Keywords")
  cat("\n")
  cat(labelsC)
sink()


}

reduceKeys <- function(tooManyKeys,desiredKeyNo){
  docTopics_short <- list()
  for (z in 1:length(tooManyKeys)){
    
    d <- tooManyKeys[[z]]
    docTopics_short[[z]] <-  d[1:desiredKeyNo]
    
  }
  
  return(docTopics_short)
  
}

