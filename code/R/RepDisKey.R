# Representative-Distinctiveness Feature Selection for Keyword Selection

# Normalisation--------------------------
docTopics.norm <- list()
for (n in names(docTopics)){
  
  d <- docTopics[[n]]
  docTopics.norm[[n]] <-  d/length(d)
  names(docTopics.norm[[n]]) <- rownames(docTopics.norm[[n]])
}

#---------------------------------------

docTopicsS <- list()
maxKeys <- 100  # select first ... terms for each doc
for (z in names(docTopics.norm)){
  
  d <- docTopics.norm[[z]]
  docTopicsS[[z]] <-  as.matrix(d[1:maxKeys])
  rownames(docTopicsS[[z]]) <- rownames(docTopics.norm[[z]])[1:maxKeys]
}

docTopics.norm <- docTopicsS

#---------------------------------------

#-------- collect all terms over document
terms <- list() # list initialisation
for (i in names(docTopics.norm)){
  terms[[i]]<- rownames(docTopics.norm[[i]])  # collect all terms for each doc
}

all.terms <- c()
for (d in names(terms)) { all.terms <- union(all.terms, terms[[d]])}   # take union of all terms in set



  D.set <- docTopics.norm[1:noOfD ]
  C.set <- docTopics.norm[(noOfD+1):(noOfD+noOfC) ]
  
  numOfD <- length(D.set)
  numOfCol <- length(C.set)
  
 #-------- Representativeness: compare features within D.set - want lowest distance overall
  
  rep.feature <- list()
  rep.values <- list()
  
  for (t in all.terms){
    
    sum.values <- c()
    
    for (j in 1:(numOfD-1)){
      
      if ((t %in% names(D.set[[j]])) == FALSE){ 
      next
      }
      d.1 <- as.double(D.set[[j]][t])
      
      
       for (jj in j+1:(numOfD-j)){
       
         if ((t %in% names(D.set[[jj]])) == FALSE){ 
           next
         }
         d.2 <- as.double(D.set[[jj]][t])
         
         dist.dd <- abs(d.1-d.2)
         sum.values <- c(sum.values,dist.dd)
        
        
      }
    }
    rep.values[[t]] <- sum.values
    
    rep.feature[t] <- 2/ (abs(numOfD)^2 - abs(numOfD))* sum(sum.values)
  }
  
  #------------Distinctiveness: compare Dickens and Collins set
  
  dist.feature <- list()
  dist.values <- list()


  
    for (t in all.terms){
    
    sum.valuesDC <- c()
    
    for (j in 1:numOfD){
      
      if ((t %in% names(D.set[[j]])) == FALSE){ 
        next
      }
      
      d.1 <- as.double(D.set[[j]][t])
      
      
      
      for (jj in 1:numOfCol){
        
        if ((t %in% names(C.set[[jj]])) == FALSE){ 
          next
        }
        c.1 <- as.double(C.set[[jj]][t])
        
        
        dist.dc <- abs(d.1 -c.1)
        print(dist.dc)
        sum.valuesDC <- c(sum.valuesDC,dist.dc)
      }
    }
    dist.values[[t]] <- sum.valuesDC
    frac <- 1
    if (numOfCol != numOfD){
      frac <- (2/ ((abs(numOfD))* (abs(numOfCol) - abs(numOfD))))
      }
    dist.feature[t] <- frac* sum(sum.valuesDC)
    
  }
  
  
  
  #------ Feature comparison
new.terms <- union(names(rep.feature),names(dist.feature))
  
  dist.all <- list()
  rep.null <- c()
  rep.extra <- c()
  feature.comp <- as.matrix(rep(0, length(1)))
  
   for (i in new.terms){
    
     if (length(rep.values[[i]])==0){
      rep.null <- c(rep.null,i)
      }else{
        if(length(dist.values[[i]])==0){
          rep.extra <- c(rep.extra,i)
        }else{
    
    
    dist.all[[i]] <- c(dist.values[[i]],rep.values[[i]])
    
    feature.comp[i] <- abs((dist.feature[[i]] - mean(dist.all[[i]]))/sd(dist.all[[i]])) - ((rep.feature[[i]]- mean(dist.all[[i]]))/sd(dist.all[[i]]))
    
  }
   
   }}
feature.comp <- feature.comp[feature.comp !=0]
  
  #------select highest no. of terms:  at the moment: everything above mean for set 
  
  mean.comp <- mean(feature.comp)
  comp.red <- c()
  for (i in 1:length(feature.comp)){
    if(feature.comp[i] > mean.comp){
      comp.red <- c(comp.red,i)
    }
  }
  
  
  return(comp.red)
  
}


