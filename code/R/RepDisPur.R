repDis <- function(matrixIn,noOfD,noOfC,noOfFeat, alpha){
  
    prim.set <- matrixIn[1:noOfD, ]
    sec.set <- matrixIn[(noOfD+1):(noOfD+noOfC), ]
  
    
  
  numOfP <- length(rownames(prim.set))
  numOfS <- length(rownames(sec.set))
  all.terms <- c(colnames(prim.set))[1:noOfFeat]
    
    primDiv <- (2/ ((numOfP)^2 - numOfP))
    secDiv <- (2/ ((numOfS^2) - numOfS))
    frac <- 1/ ((numOfP)* ((numOfS+numOfP) - numOfP))
 
  #-------- Representativeness: compare features within both sets
  
  rep.feature <- list()
  rep.values <- list()
  
  rep2.feature <- list()
  rep2.values <- list()
  
  dist.feature <- list()
  dist.feature.2 <- list()
  dist.values <- list()
  

  for (t in all.terms){
    print(t)
    
    sum.values <- c()
    
    #primary set
    for (j in 1:(numOfP-1)){
      
      if ((prim.set[j,t]) == 0){next}
      
      d.1 <- as.double((prim.set[j,t]))
      
      for (jj in j+1:(numOfP-j)){
        
        if ((prim.set[jj,t]) == 0){next}
        d.2 <- as.double((prim.set[jj,t]))
        
        dist.dd <- abs(d.1-d.2)
        sum.values <- c(sum.values,dist.dd)
       }
    }
    
    rep.values[[t]] <- sum.values
    rep.feature[t] <- primDiv* sum(sum.values)
   
    
    
    
    # secondary set
    sum.valuesC <- c()
    for (g in 1:(numOfS-1)){
      
      if ((sec.set[g,t]) == 0){ next}
      c.1 <- as.double((sec.set[g,t]))
      
      for (gg in g+1:(numOfS-g)){
        
        if ((sec.set[gg,t]) == 0){ next}
        
        c.2 <- as.double((sec.set[gg,t]))
        dist.cc <- abs(c.1-c.2)
        sum.valuesC <- c(sum.valuesC,dist.cc)
      }
    }
    
    
    rep2.values[[t]] <- sum.valuesC
    rep2.feature[t] <- secDiv* sum(sum.valuesC)
    
    
    
    # mixed set
    sum.valuesDC <- c()
    
    for (l in 1:numOfP){
      
      if ((prim.set[l,t]) == 0){ next}
      
     d.2 <- as.double((prim.set[l,t]))
     for (ll in 1:numOfS){
        
        if ((sec.set[ll,t]) == 0){ next}
          c.2 <- as.double((sec.set[ll,t]))
        
        dist.dc <- abs(d.2 -c.2)
        sum.valuesDC <- c(sum.valuesDC,dist.dc)
      }
    }
  
   
    
      dist.values[[t]] <- sum.valuesDC
     
      dist.feature[t] <- frac* sum(sum.valuesDC)
    dist.feature.2[t] <- frac* sum(sum.valuesDC)
    
  }
  
  
  
  print("Features done!")
  
  
  #------ Feature comparison
  new.terms.1 <- union(names(rep.feature),names(dist.feature))
  new.terms.2 <- union(names(rep2.feature),names(dist.feature))
  
  dist.all <- list()
  dist.all.2 <- list()
  feature.1 <- as.matrix(rep(0, length(1)))
  feature.2 <- as.matrix(rep(0, length(1)))
  
  #-----Dickens set
  
  for (i in new.terms.1){
        if (i %in% names(dist.values) && i %in% names(rep.values)){
  
        dist.all[[i]] <- c(dist.values[[i]],rep.values[[i]])
        feature.1[i] <- abs(((dist.feature[[i]] - mean(dist.all[[i]]))/sd(dist.all[[i]])) - ((rep.feature[[i]]- mean(dist.all[[i]]))/sd(dist.all[[i]])))
       }
}
  
  feature.1 <- feature.1[feature.1 !="NaN"]
  feature.1 <- feature.1[feature.1 !=0]
  
  #--- same for Collins/other set
  for (i in new.terms.2){
    
    if (i %in% names(dist.values) && i %in% names(rep2.values)){
    dist.all.2[[i]] <- c(dist.values[[i]],rep2.values[[i]])
    
    feature.2[i] <- abs(((dist.feature.2[[i]] - mean(dist.all.2[[i]]))/sd(dist.all.2[[i]])) - ((rep2.feature[[i]]- mean(dist.all.2[[i]]))/sd(dist.all.2[[i]])))
  }
  }
  
 feature.2 <- feature.2[feature.2 !="NaN"]
  feature.2 <- feature.2[feature.2 !=0]
  
  #------select highest no. of terms:  at the moment: everything above mean for set 
  
  mean.1 <- mean(feature.1)
  sd.1 <- sd(feature.1)
  feature.1.red <- as.matrix(feature.1[feature.1 > ((mean.1+sd.1)*alpha)])
   
  mean.2 <- mean(feature.2)
  sd.2 <- sd(feature.2)
  feature.2.red <- as.matrix(feature.2[feature.2 > ((mean.2+sd.2)*alpha)])
  
  values <- list()
  values[["features.orig"]] <- feature.1
  values[["features.orig.2"]] <- feature.2
    
  values[["features.1"]] <- feature.1.red
  values[["features.2"]] <- feature.2.red
   return(values)
  
}
