repDis <- function(matrixIn,noOfD,noOfC,setToTest){
  

  #-------- collect all terms over document
  
  dickens.set <- matrixIn[1:noOfD, ]
  collins.set <- matrixIn[(noOfD+1):(noOfD+noOfC), ]
  
  
  if (setToTest==1){
    
    prim.set <- dickens.set
    sec.set <- collins.set
  }else{
    prim.set <- collins.set
    sec.set <- dickens.set
  }
  numOfP <- length(rownames(prim.set))
  numOfS <- length(rownames(sec.set))
  
  all.terms <- c(colnames(prim.set))[1:3] # list initialisation
 # for (i in rownames(prim.set)){
  #  terms[[i]]<- colnames(prim.set[[i],])  # collect all terms for each doc}
  
  #all.terms <- c()
  #for (d in names(terms)) { all.terms <- union(all.terms, terms[[d]])}   # take union of all terms in set
  
  #all.terms <- all.terms(1:500)
  
  
  #-------- Representativeness: compare features within D.set - want lowest distance overall
  
  rep.feature <- list()
  rep.values <- list()
  
  dist.feature <- list()
  dist.values <- list()
  distance.docs <- list()
  labels.P <- rownames(prim.set)
  labels.S <- rownames(sec.set)
  
  for (t in all.terms){
    print(t)
    doc.by.doc <- matrix(0, nrow=length(rownames(matrixIn)),ncol=length(rownames(matrixIn)))
    rownames(doc.by.doc) <- rownames(matrixIn)
    colnames(doc.by.doc) <- rownames(matrixIn)
    sum.values <- c()
    
    for (j in 1:(numOfP-1)){
      
      if ((prim.set[j,t]) == 0){ 
        next
      }
      d.1 <- as.double(log(prim.set[j,t]))
      
      
      
      for (jj in j+1:(numOfP-j)){
        
        if ((prim.set[jj,t]) == 0){ 
          next
        }
        
         d.2 <- as.double(log(prim.set[jj,t]))
        
        dist.dd <- abs(d.1-d.2)
        doc.by.doc[labels.P[j],labels.P[jj]] <- dist.dd
         
        sum.values <- c(sum.values,dist.dd)
       }
    }
    rep.values[[t]] <- sum.values
  
    
    rep.feature[t] <- (2/ (abs(numOfP)^2 - abs(numOfP)))* sum(sum.values)
    
    sum.valuesDC <- c()
    
    for (l in 1:numOfP){
      
      if ((prim.set[l,t]) == 0){ 
        next
      }
      
     d.2 <- as.double(log(prim.set[l,t]))
     
     
      
      for (ll in 1:numOfS){
        
        if ((sec.set[ll,t]) == 0){ 
          next
        }
          c.2 <- as.double(log(sec.set[ll,t]))
        
        
        dist.dc <- abs(d.2 -c.2)
        print(dist.dc)
        doc.by.doc[labels.P[l],labels.S[ll]] <- dist.dc
        sum.valuesDC <- c(sum.valuesDC,dist.dc)
      }
    }
    dist.values[[t]] <- sum.valuesDC
    frac <- 1
    if (numOfS != numOfP){
      frac <- (2/ ((abs(numOfP))* (abs(numOfS) - abs(numOfP))))
    }
    dist.feature[t] <- frac* sum(sum.valuesDC)
    distance.docs[[t]] <- doc.by.doc
    
  }
    
  print("Features done!")
  
  
  #------ Feature comparison
  new.terms <- union(names(rep.feature),names(dist.feature))
  
  dist.all <- list()
 
  feature.comp <- as.matrix(rep(0, length(1)))
  
  for (i in new.terms){
    
   
        
        dist.all[[i]] <- c(dist.values[[i]],rep.values[[i]])
        
        feature.comp[i] <- abs(((dist.feature[[i]] - mean(dist.all[[i]]))/sd(dist.all[[i]])) - ((rep.feature[[i]]- mean(dist.all[[i]]))/sd(dist.all[[i]])))
        print(feature.comp[i])
      }
      
    feature.comp <- feature.comp[feature.comp !="NaN"]
  feature.comp <- feature.comp[feature.comp !=0]
  
  #------select highest no. of terms:  at the moment: everything above mean for set 
  
  mean.comp <- mean(feature.comp)
  comp.red <- as.matrix(feature.comp[feature.comp > mean.comp])
  
  print(comp.red)
  
  vals <- list()
  vals[["dist.F"]] <- comp.red 
  vals[["dist.v"]] <- dist.values
  vals[["rep.v"]] <- rep.values
  
   return(vals)
  
}
