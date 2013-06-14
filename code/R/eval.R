#----evaluation
library(fastICA)


evalICA <- function(nV){

#parameters
numOfIC <- 84  # set no. of comp
keyThres <- 0.1
compThres <- 1.0
maxTerms <- 100 # to be set according to max number of desired keywords
#---
  
#----------------------------
D.diff <- list()
C.diff <- list()
numOfD <- 86


for (i in 1:numOfD){

print(i)
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

compT <- A 
docs <- S

colnames(docs) <- paste(1:numOfIC)   # name components
docnames <- rownames(nV)

colnames(compT) <- colnames(nV)  # get term names
compLst <- list()
compLst.neg <- list()

#-------------------------create list where for each comp - list of keyword-weight pair 

for(i in 1:numOfIC) {   # sort keywords in components according to weight
  
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
  
  pos.comp <- comp.ord[comp.ord > compThres] 
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
  keylist <- keylist[keylist >0]
  docTopics[[n]] <- as.matrix(keylist)
  
} 


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


# TODO remove those keywords in both sets? 

# TODO extend maxTerms and select best keywords with Rep.Dis

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

diff <- list()
diff[["Dickens"]] <- D.diff
diff[["Collins"]] <- C.diff


return(diff)
}