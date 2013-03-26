

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
    compkeys <- list()
    for (l in 1:keysize){
      
      compkey <- keyplusweight[[1]][[l]][[1]] # extract all keys for curr comp
      keyWeight <- as.double(keyplusweight[[1]][[l]][[2]]) # extract weight for key in comp
      keyWeight <- keyWeight*compWeight # calc importance of key in doc 
     
      if (is.null(keylist[[compkey]])){   # if key not there yet, add to list for doc + weight
        keylist[[compkey]] <- keyWeight   # assign weight to keyword
        }else{
          
          oldWeight <- keylist[[compkey]]  # get previous value
          newWeight <- oldWeight+keyWeight # add additional weight 
          keylist[[compkey]] <- newWeight # assign new value
        }
      
      }
    
 }
  docTopics[[docnames[i]]] <- keylist
  
}






