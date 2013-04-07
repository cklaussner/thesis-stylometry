#----evaluation
library(fastICA)


#parameters
numOfIC <- 85  # set no. of comp
keyThres <- 0.1
compThres <- 2.0
maxTerms <- 70 # to be set according to max number of desired keywords
---
  
Msize <- dim(nV)
numOfDocs <- Msize[1] 
numOfComps <- Msize[2] 
#----------------------------
D.diff <- list()
C.diff <- list()


for (i in 1:numOfDocs){

test.set <- nV[i,] # extract doc for test
remove.doc <- rownames(nV)[i]
train.set <- nV[!rownames(nV) %in% remove.doc, ] # create new matrix with document left out


Y<- fastICA(train.set, numOfIC, alg.typ = "deflation",
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
numOfComps <- Ssize[2] 

compT <- abs(A) # get absolute vals since ica model is ambiguous in regard to sign 
docs <- abs(S)

colnames(docs) <- paste(1:numOfIC)   # name components
docnames <- rownames(nV)

colnames(compT) <- colnames(nV)  # get term names
compLst <- list()

#-------------------------create list where for each comp - list of keyword-weight pair 

for(i in 1:numOfIC) {   # sort keywords in components according to weight
  key <- sort(compT[i, ],decreasing = TRUE) # sort comp. according to most prominent keywords
  label <- names(key) # get term names
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

for (i in 1:length(compLst)){
  if (length(compLst[[i]])==0){
    print(i)
    print("Warning - no keywords for comp")}}

#---------------------------# same for components for each doc - order according to weight - pos in list = pos in doc

docLst <- list()
for (i in 1:numOfDocs){   # order comp for each document
  
  c_ord <- sort(docs[i, ],decreasing = TRUE)  # sort accord. to weight
  
  label <- names(c_ord) # get comp names
  names(c_ord) <- NULL # rmv comp names
  lst <- list()
  
  for (j in 1:numOfIC){ 
    
    if(c_ord[j] < compThres){
      break
    }
    lst[[j]] <- cbind(label[j],c_ord[j])
  }
  
  docLst[[docnames[i]]] <-  as.list(lst)    # add to overall comp list
}

for (i in 1:length(docLst)){
  
  if (length(docLst[[i]])==0){
    print(i)
    print("Warning - no comp for doc")
  }}

docTopics <- list()

for (i in 1:numOfDocs){   # get keywords for each document
  docKeys <- list()
  
  complist <- as.list(docLst[i]) # get component list for each doc
  
  csize <- length(complist[[1]]) # get no of comp for doc
  
  cmplist <- list()
  keylist <- list() # list for keywords 
  for (j in 1:csize){ # for all comp for doc
    
    cmp <- as.integer(complist[[1]][[j]][[1]])  # extract comp no. so we can get keywords and weights 
    compWeight <- as.double(complist[[1]][[j]][[2]]) # extract weight for comp in doc
    
    
    keyplusweight <-as.list(compLst[cmp]) # get keywords for comp.
    
    keysize <- length(keyplusweight[[1]]) # get no. of terms
    if (keysize!=0){
      compkeys <- list()
      for (l in 1:keysize){
        
        compkey <- keyplusweight[[1]][[l]][[1]] # extract all keys for curr comp
        keyWeight <- as.double(keyplusweight[[1]][[l]][[2]]) # extract weight for key in comp
        keyWeight <- keyWeight*(2*compWeight) # calc importance of key in doc 
        
        if (is.null(keylist[[compkey]])){   # if key not there yet, add to list for doc + weight
          keylist[[compkey]] <- keyWeight   # assign weight to keyword
        }else{
          
          oldWeight <- keylist[[compkey]]  # get previous value
          newWeight <- oldWeight+keyWeight # add additional weight 
          keylist[[compkey]] <- newWeight # assign new value
        }
        
      }}
    
  }
  docTopics[[docnames[i]]] <- keylist
  
}
# ----------------------- get overall word frequencies 

docLength <- length(docTopics) # no. of Docs
terms <- list() # list initialisation
for (i in 1:docLength){
  terms[[names(docTopics[i])]]<- names(docTopics[[i]])  # collect all terms for each doc
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


finalKeysD <- as.matrix(terms.order)[1:maxTerms,1]
finalKeysC <- as.matrix(termsS.order)[1:maxTerms,1]

#-----eval. for current keyword list + "missing" doc. 

#--------------------Test agains Dickens' keywords


#--- Dickens.set
termsize.D <- sum(finalKeysD)
hist.D <-as.matrix(rep(0, length(finalKeysD)))
rownames(hist.D) <- names(finalKeysD)
for (n in names(finalKeysD)){
  hist.D[n,] <- finalKeysD[[n]]/termsize.D
  
}

### extract Dickens keywords from test set vector
test.vec <- as.matrix(rep(0, length(finalKeysD)))
rownames(test.vec) <- names(finalKeysD)
for (term in names(finalKeysD)){

  test.vec[term,] <- test.set[[term]] 
}
termsize.T <- sum(test.vec)

hist.test <- as.matrix(rep(0, length(finalKeysD)))
rownames(hist.test) <- names(finalKeysD)
for (n in rownames(test.vec)){
  hist.test[n,] <- test.vec[n,]/termsize.T
}

#---compare loop
abs.diff <- c()
for (c in rownames(hist.D)){
  val <- abs(hist.D[c,] - hist.test[c,])
  
  abs.diff <- union(abs.diff,val)
}
abs.diff <- sum(abs.diff)


#--------------------Test agains Collins' keywords

#--- Collins.set
termsize.C <- sum(finalKeysC)
hist.C <- as.matrix(rep(0, length(finalKeysC)))
rownames(hist.C) <- names(finalKeysC)
for (n in names(finalKeysC)){
  hist.C[n,] <- finalKeysC[[n]]/termsize.C
  
}

### extract Collins keywords from test set vector
test.vec2 <- as.matrix(rep(0, length(finalKeysC)))
rownames(test.vec2) <- names(finalKeysC)
for (term in names(finalKeysC)){
  
  test.vec2[term,] <- test.set[[term]] 
}
termsize.T2 <- sum(test.vec2)

hist.test2 <- as.matrix(rep(0, length(finalKeysC)))
rownames(hist.test2) <- names(finalKeysC)
for (n in rownames(test.vec2)){
  hist.test2[n,] <- test.vec2[n,]/termsize.T2
}

#---compare loop for Collins
abs.diff2 <- c()
for (c in rownames(hist.C)){
  val2 <- abs(hist.C[c,] - hist.test2[c,])
  
  abs.diff2 <- union(abs.diff2,val2)
}
abs.diff2 <- sum(abs.diff2)


D.diff[[remove.doc]] <- abs.diff
C.diff[[remove.doc]] <- abs.diff2
}
