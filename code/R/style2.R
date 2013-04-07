
# -----extract comp. keywords, write to file
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





#for (i in 1:length(docLst)){print(length(docLst[[i]]))}

#for (i in 1:length(compLst)){print(length(compLst[[i]]))}

#----get keywords for each doc 2nd VERSION!!!

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


#--------------write to output file

sink("outfile.txt")
for(s in 1:numOfDocs){
  
  d <- docTopics_short[[s]]
  ds <- paste(names(d),collapse=", ")
  cat("\n")
  cat(docnames[s])
  cat("\n")
  cat(ds)
}
sink()




