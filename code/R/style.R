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

numOfIC <- 85   # set no. of comp
keyThres <- 0.03
compThres <- 1.0


Y<- fastICA(nV, numOfIC, alg.typ = "deflation",
         fun = "exp", alpha = 1.0, method = "R",
           row.norm = TRUE, maxit = 200, tol = 1e-04, verbose = TRUE,
          w.init = NULL)

S <- Y$S # extract doc-by-comp 
A <- (Y$A) # get inverse of unmixing matrix W --> weights for terms of each comp.

Asize <- dim(A)
numOfComps <- Asize[1] 
numOfTerms <- Asize[2] 

Ssize <- dim(S)
numOfDocs <- Ssize[1] 
numOfComps <- Ssize[2] 

compT <- abs(A) # get absolute vals since ica model is ambiguous in regard to sign 
docs <- abs(S)

colnames(docs) <- paste(1:noOfIC)   # name components
docnames <- rownames(docs)

colnames(compT) <- colnames(nV[2:length(nV)])  # get term names
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


#----------------------------------


docTopics <- list()

for (i in 1:numOfDocs){   # get keywords for each document
  docKeys <- list()
  complist <- as.list(docLst[i]) # get component list for each doc
  
  csize <- length(complist[[i]]) # get no of comp for doc

  cmplist <- list()
  for (j in 1:csize){ # for all comp for doc
    
    cmp <- as.integer(complist[[1]][[j]][[1]])  # extract comp no. so we can get keywords - complist[[1]][[j]][[2]] gets weight
    
    keyplusweight <-as.list(compLst[cmp]) # get keywords for comp.
    
    keysize <- length(keyplusweight[[1]]) # get no. of terms
    compkeys <- list()
    for (l in 1:keysize){
      
      compkeys[[l]] <- keyplusweight[[1]][[l]][[1]] # extract all keys for curr comp
      }
    
    docKeys[[j]] <-compkeys 
    
    }
  docTopics[[docnames[i]]] <- docKeys
  
  }


write.csv(docTopics,"docTopics.csv")

#d <- compLst[[1]]
#> d[[1]]
#[,1]    [,2]               
#[1,] "mercy" "0.484248975027599"
#> d[[1]][1]
#[1] "mercy"

  


#(sort(col_sums((dtm)),decreasing = TRUE)) 





