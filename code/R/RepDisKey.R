# Representative-Distinctiveness Feature Selection for Keyword Selection


repDisKey <- function(docTopics,noOfD,noOfC,setToTest){
# Normalisation--------------------------
docTopics.norm <- list()
for (n in names(docTopics)){
  
  d <- docTopics[[n]]
  docTopics.norm[[n]] <-  d/length(d)
  names(docTopics.norm[[n]]) <- rownames(docTopics.norm[[n]])
}

#---------------------------------------

#docTopicsS <- list()
#maxKeys <- 100  # select first ... terms for each doc
#for (z in names(docTopics.norm)){
  
 # d <- docTopics.norm[[z]]
  #docTopicsS[[z]] <-  as.matrix(d[1:maxKeys])
  #rownames(docTopicsS[[z]]) <- rownames(docTopics.norm[[z]])[1:maxKeys]}

#docTopics.norm <- docTopicsS


#-------- collect all terms over document

terms <- list() # list initialisation
for (i in names(docTopics.norm)){
  terms[[i]]<- rownames(docTopics.norm[[i]])  # collect all terms for each doc
}

all.terms <- c()
for (d in names(terms)) { all.terms <- union(all.terms, terms[[d]])}   # take union of all terms in set

dickens.set <- docTopics.norm[1:noOfD ]
collins.set <- docTopics.norm[(noOfD+1):(noOfD+noOfC) ]


if (setToTest==1){
  
  prim.set <- dickens.set
  sec.set <- collins.set
}else{
  prim.set <- collins.set
  sec.set <- dickens.set
}

  
  
  numOfP <- length(prim.set)
  numOfS <- length(sec.set)
  
 #-------- Representativeness: compare features within D.set - want lowest distance overall
  
  rep.feature <- list()
  rep.values <- list()
  
  for (t in all.terms){
    
    sum.values <- c()
    
    for (j in 1:(numOfP-1)){
      
      if ((t %in% names(prim.set[[j]])) == FALSE){ 
      next
      }
      d.1 <- as.double(prim.set[[j]][t])
      
      
       for (jj in j+1:(numOfP-j)){
       
         if ((t %in% names(prim.set[[jj]])) == FALSE){ 
           next
         }
         d.2 <- as.double(prim.set[[jj]][t])
         
         dist.dd <- abs(d.1-d.2)
         sum.values <- c(sum.values,dist.dd)
        
        
      }
    }
    rep.values[[t]] <- sum.values
    
    rep.feature[t] <- 2/ (abs(numOfP)^2 - abs(numOfP))* sum(sum.values)
  }
  
  #------------Distinctiveness: compare Dickens and Collins set
  
  dist.feature <- list()
  dist.values <- list()


  
    for (t in all.terms){
    
    sum.valuesDC <- c()
    
    for (j in 1:numOfP){
      
      if ((t %in% names(prim.set[[j]])) == FALSE){ 
        next
      }
      
      d.1 <- as.double(prim.set[[j]][t])
      
      
      
      for (jj in 1:numOfS){
        
        if ((t %in% names(sec.set[[jj]])) == FALSE){ 
          next
        }
        c.1 <- as.double(sec.set[[jj]][t])
        
        
        dist.dc <- abs(d.1 -c.1)
        
        sum.valuesDC <- c(sum.valuesDC,dist.dc)
      }
    }
    dist.values[[t]] <- sum.valuesDC
    frac <- 1
    if (numOfS != numOfP){
      frac <- (2/ ((abs(numOfP))* (abs(numOfS) - abs(numOfP))))
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
  comp.red <- as.matrix(feature.comp[feature.comp > ((7/4)*mean.comp)])

  print(comp.red)
 
  vals <- list()
  vals[["dist.F"]] <- comp.red 
  vals[["rep.ex"]] <- rep.extra 
  return(vals)
  
}


