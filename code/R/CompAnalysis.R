
#Component Analysis


numOfIC <- 40 # set no. of comp
keyThres <- 0.0
compThres <- 0.0



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

compKeys <- list()

for (i in 1:length(compLst)){
  cv <- c()
  if(length(compLst[[i]])!= 0){
    cmp <- compLst[[i]]
    
    for(j in 1:length(cmp)){
      cv <- c(cv,cmp[[j]][[1]][[1]])
    }}
  compKeys[[i]] <- cv
}

docComps <- list()

for (i in 1:length(docLst)){
  cd <- c()
  if(length(docLst[[i]])!= 0){
    doc <- docLst[[i]]
    
    for(j in 1:length(doc)){
      cd <- c(cd,as.integer(doc[[j]][[1]][[1]]))
    }}
  n <- names(docLst)[i]
  docComps[[n]] <- cd
}

docLength <- length(docComps) # no. of Docs
comps <- list() # list initialisation
for (i in 1:docLength){
  comps[[names(docComps[i])]] <- docComps[[i]]  # collect all terms for each doc
}

all.comps <- c()
for (d in names(comps)) { all.comps <- union(all.comps, comps[[d]])}   # take union of all terms in set

comps.count <- as.matrix(rep(0, length(all.comps)))
comps.countS <- as.matrix(rep(0, length(all.comps)))
rownames(comps.count) <- all.comps
rownames(comps.countS) <- all.comps
for (d in names(comps)) {
  if (substr(d,1,1) == "D"){
    d.count <- as.matrix(xtabs(~comps[[d]]))
    comps.count[rownames(d.count),] <- comps.count[rownames(d.count),] + d.count[,1]
    
  }else{ if(substr(d,1,1) == "W"){
    dS.count <- as.matrix(xtabs(~comps[[d]]))
    comps.countS[rownames(dS.count),] <- comps.countS[rownames(dS.count),] + dS.count[,1]
  }
  }}

comps.order <- as.matrix(comps.count[order(comps.count[,1],decreasing = TRUE ),])   
rownames(comps.order) <- paste("C",rownames(comps.order))
compsS.order  <- as.matrix(comps.countS[order(comps.countS[,1],decreasing = TRUE ),])
rownames(compsS.order) <- paste("C",rownames(compsS.order))

#----write to file

sink("compKeys.txt")
for(s in 1:length(compLst)){
  
  d <- compKeys[[s]]
  ds <- paste(d,collapse=", ")
  cat("\n")
  cat(s)
  cat("\n")
  cat(ds)
  
}
cat("\n")
cat("Dickens")
c(paste(rownames(comps.order),comps.order, collapse = ":"))
cat("\n")
cat("Collins")
c(paste(rownames(compsS.order), compsS.order, collapse = ":"))
cat("\n")
for(s in 1:length(docLst)){
  
  d <- docComps[[s]]
  ds <- paste(d,collapse=", ")
  cat("\n")
  cat(names(docLst)[s])
  cat("\n")
  cat(ds)
}
sink()



