library(tm)  
library (plyr)
library("slam")

source("preprocessing/prepMat.R")
source("ica/rmvMean.R")
source("ica/svdMat.R")
source("ica/pcamat.R")
source("ica/ica.R")
source("ica/calcIC.R")
####"/home/carmen/Dropbox/Thesis/Data/text/SpecializedSet/"


args <- commandArgs(TRUE)
loc <- (args[1])  


################################## text preprocessing  #######################################

dtm <- prepMat(loc)   # dtm with tf weighting
spDtm <- removeSparseTerms(dtm, 0.4) # 2nd argument indication of sparsity in matrix 


######################################### ICA ####################################

# input is document term matrix with some kind of frequency weighting ####TODO experiment with different weighting schemes

#############   centering  ############

#dtmCent <- rmvMean(spDtm)
#mixedMean <- getFeatureMean()

############ whitening ############## 

#pc <- svdMat(dtmCent)  # calculate new components with svd 
#xwhiten <- getwhiteningMatrix()

#pc <-pcamat(dtmCent) # also works, but svd gives better approx.
############# ica ###################


#ics <- ica(pc,xwhiten, 4)
#icasig <- calcIC(spDtm,ics,mixedMean)

numOfIC <- 86  # set no. of comp
keyThres <- 0.1
compThres <- 1.0


Y<- fastICA(nV, numOfIC, alg.typ = "deflation",
         fun = "exp", alpha = 1.0, method = "R",
           row.norm = TRUE, maxit = 200, tol = 1e-04, verbose = TRUE,
          w.init = NULL)

S <- Y$S # extract doc-by-comp 
A <- (Y$A) # get inverse of unmixing matrix W --> weights for terms of each comp.

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

docTopics.neg <- list()

for (n in names(docLst.neg)){   # get keywords for each document
  
  complist.neg <- docLst.neg[[n]] # get component list for each doc
  
  keylist.neg <- as.matrix(rep(0, numOfTerms)) # overall list for keywords for document - begin with entry for all terms possible
  rownames(keylist.neg) <- colnames(nV)
  
  for (c in rownames(complist.neg)){ # for all comp for doc
    
    compweight.neg <- as.double(complist.neg[c,])
    
    keys.neg <- compLst.neg[[as.integer(c)]] # get keywords for comp. 
    
    if ((length(keys.neg))!=0){
      
      keyweight.neg <- keys.neg*(2*compweight.neg) # raise all keys in comp by weight it has in document (both negative => positive again)
      
      keylist.neg[rownames(keyweight.neg),] <- keylist[rownames(keyweight.neg),] + keyweight.neg[,1]
      
    }
    
  }
  names(keylist.neg) <- rownames(keylist.neg)
  keylist.neg <- keylist.ng[keylist.neg >0]
  docTopics.neg[[n]] <- as.matrix(keylist.neg)
  
}



maxTerms <- 70
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

}





