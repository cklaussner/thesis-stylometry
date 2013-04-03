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
keyThres <- 0.1
compThres <- 2.0


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

colnames(docs) <- paste(1:numOfIC)   # name components
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
  
  csize <- length(complist[[1]]) # get no of comp for doc

  cmplist <- list()
  for (j in 1:csize){ # for all comp for doc
    
    cmp <- as.integer(complist[[1]][[j]][[1]])  # extract comp no. so we can get keywords - complist[[1]][[j]][[2]] gets weight
    
    keyplusweight <-as.list(compLst[cmp]) # get keywords for comp.
    
    keysize <- length(keyplusweight[[1]]) # get no. of terms
    compkeys <- list()
    for (l in 1:keysize){
      
      compkeys[[l]] <- keyplusweight[[1]][[l]][[1]] # extract all keys for curr comp
      }
    
    docKeys <-c(docKeys,compkeys)
    
    }
  docTopics[[docnames[i]]] <- docKeys
  
  }



#-------------------# keep only first...keywords
docTopics_short <- list()
for (z in 1:numOfDocs){
  
  d <- docTopics[[z]]
  docTopics_short[[z]] <-  d[1:100]
  
}
#--------------------------------------

# ----------check intersection of different sets, maybe make as function with set length as args

Dset <- docTopics[1:55]
Cset <- docTopics[56:86]

DsetL <- length(Dset)
CsetL <- length(Cset)

# iterate over each set:

#----Dickens
terms_intsctD <- names(Dset[[1]])

for (i in 2:DsetL){
  
  new_set <- names(Dset[[i]])
  terms_intsctD <- intersect(terms_intsctD,new_set)
}

# ----Collins
terms_intsctC <- names(Cset[[1]])

for (i in 2:CsetL){
  
  new_set <- names(Cset[[i]])
  terms_intsctC <- intersect(terms_intsctC,new_set)
}

# -------- mixed take half of each set

newDsetL <- as.integer(DsetL/2)
newCsetL <- as.integer(CsetL/2)


DsetRed <- Dset [1:newDsetL]
CsetRed <- Cset[1:newDsetL]


compSet <- c(DsetRed,CsetRed)
CompsetL <- length(compSet)
terms_intsctM <- names(compSet[[1]])

for (i in 2:CompsetL){
  
  new_set <- names(compSet[[i]])
  terms_intsctM <- intersect(terms_intsctM,new_set)
}


#alternative version
d.terms <- all.terms
for (d in names(terms)) { 
  if (substr(d,1,1) == "D") { # take only documents whose first letter is 'd'
    d.terms <- intersect(d.terms, terms[[d]])
    print(d.terms)
  }
}

#------------------------------intersection check



# ----------------------- get overall word frequencies 
maxTerms <- 20 # to be set according to max number of desired keywords
docLength <- length(docTopics) # no. of Docs
terms <- list() # list initialisation
for (i in 1:docLength){
  terms[[names(docTopics[i])]]<- names(docTopics[[i]])  # collect all terms for each doc
}

all.terms <- c()
for (d in names(terms)) { all.terms <- union(all.terms, terms[[d]])}   # take union of all terms in set

#d.terms <- all.terms
#for (d in names(terms)) { 
 # print(substr(d,1,1))
  #if (substr(d,1,1) == "D") { # take only documents whose first letter is 'D'
  #  d.terms <- c(d.terms,intersect(d.terms, terms[[d]]))
  #}}

terms.count <- as.matrix(rep(0, length(all.terms)))
terms.countS <- as.matrix(rep(0, length(all.terms)))
rownames(terms.count) <- all.terms
rownames(terms.countS) <- all.terms
for (d in names(terms)) {
  if (substr(d,1,1) == "D"){
  d.count <- as.matrix(xtabs(~terms[[d]]))
  terms.count[rownames(d.count),] <- terms.count[rownames(d.count),] + d.count[,1]
  print(d.count[1,1])
}else{ if(substr(d,1,1) == "W"){
  dS.count <- as.matrix(xtabs(~terms[[d]]))
  terms.countS[rownames(dS.count),] <- terms.countS[rownames(dS.count),] + dS.count[,1]
  
  
}
  }}

terms.order <- terms.count[order(terms.count[,1],decreasing = TRUE ),]    # order doc sets according to freq.
termsS.order  <- terms.countS[order(terms.countS[,1],decreasing = TRUE ),]


finalKeysD <- as.matrix(terms.order)[1:maxTerms,1]
finalKeysC <- as.matrix(terms.orderS)[1:maxTerms,1]

#------------------ feature stability 
terms.list <- list()
terms.mean <- list()
terms.std <- list()


for (i in 1:length(docTopics)){
  doc <- docTopics[[i]]
  # put in check for docs of right set
  for (term in names(doc)){
    
    
    if (is.null(terms.list[[term]])){
      terms.list[[term]] <- c(doc[[term]])  # put in first weight for term
    }else{
     terms.list[[term]] <- c(terms.list[[term]], doc[[term]])
     
      
    } 
  }
}
     
for (key in names(terms.list)){
  
  terms.mean[[key]] <- mean(terms.list[[key]])
  terms.std[[key]] <- sd(terms.list[[key]]) 
}
#------------Weighting of freq and stability

terms.weight <- list()

for (key in names(terms.list)){
  
  terms.weight[[key]] <- sum(terms.list[[key]])*1/terms.std[[key]]
}

#--------------write to output file

sink("outfile.txt")
for(s in 1:numOfDocs){
  
  d <- unique(docTopics_short[[s]])
  ds <- paste(d,collapse= ' , ')
  cat("\n")
  cat(docnames[s])
  cat("\n")
  cat(ds)
}
sink()





