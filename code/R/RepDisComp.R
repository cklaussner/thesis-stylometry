# Representative-Distinctiveness Feature Selection

repDisComp <- function(S,noOfD,noOfC,alpha){   # input is doc-comp matrix S, num of Dickens doc and Collins, assuming D. comes first

  prim.set <- S[1:noOfD, ]
  sec.set <- S[(noOfD+1):(noOfD+noOfC), ]
  
  numOfP <- length(rownames(prim.set))
  numOfS <- length(rownames(sec.set))
  
  
  Ssize <- dim(S)
  numOfComps <- Ssize[2] 

#-------- Representativeness: compare features within D.set - want lowest distance overall

  rep.feature <- list()
  rep.values <- list()
  
  rep2.feature <- list()
  rep2.values <- list()
  
  dist.feature <- list()
  dist.feature.2 <- list()
  dist.values <- list()

  
for (i in 1:numOfComps){
  sum.values <- c()
  
  
  
#primary set
for (j in 1:(numOfP-1)){
  
  if ((prim.set[j,i]) == 0){ 
    next
  }
  d.1 <- as.double(prim.set[j,i])
  
  for (jj in j+1:(numOfP-j)){
    
    if ((prim.set[jj,i]) == 0){ 
      next
    }
    d.2 <- as.double(prim.set[jj,i])
    
    dist.dd <- abs(d.1-d.2)
    sum.values <- c(sum.values,dist.dd)
  }
}

rep.values[[i]] <- sum.values
rep.feature[[i]] <- (2/ (abs(numOfP)^2 - abs(numOfP)))* sum(sum.values)


  # secondary set
  sum.valuesC <- c()
  for (g in 1:(numOfS-1)){
    
    if ((sec.set[g,i]) == 0){ 
      next
    }
    c.1 <- as.double(sec.set[g,i])
    
    for (gg in g+1:(numOfS-g)){
      
      if ((sec.set[gg,i]) == 0){ 
        next
      }
      
      c.2 <- as.double(sec.set[gg,i])
      dist.cc <- abs(c.1-c.2)
      sum.valuesC <- c(sum.valuesC,dist.cc)
    }
  }
  rep2.values[[i]] <- sum.valuesC
  rep2.feature[i] <- (2/ ((numOfS^2) - numOfS))* sum(sum.valuesC)
  

  
  # mixed set
  sum.valuesDC <- c()
  
  for (l in 1:numOfP){
    
    if ((prim.set[l,i]) == 0){ 
      next
    }
    
    d.2 <- as.double(prim.set[l,i])
    for (ll in 1:numOfS){
      
      if ((sec.set[ll,i]) == 0){ 
        next
      }
      c.2 <- as.double(sec.set[ll,i])
      
      dist.dc <- abs(d.2 -c.2)
      sum.valuesDC <- c(sum.valuesDC,dist.dc)
    }
  }
  dist.values[[i]] <- sum.valuesDC
  
  
  frac <- 1/ ((numOfP)* ((numOfS+numOfP) - numOfP))
 
  
  
  dist.feature[i] <- frac* sum(sum.valuesDC)
  
  dist.feature.2[i] <- frac* sum(sum.valuesDC)
}
  
 
  
  print("Features done!")
  
  
  #------ Feature comparison
  #new.terms.1 <- union(names(rep.feature),names(dist.feature))
  #new.terms.2 <- union(names(rep2.feature),names(dist.feature))
  
  dist.all <- list()
  dist.all.2 <- list()
  feature.1 <- as.matrix(rep(0, numOfComps))
  names(feature.1) <- c(1:numOfComps)
  feature.2 <- as.matrix(rep(0, numOfComps))
  names(feature.2) <- c(1:numOfComps)
  #-----Dickens set
  
  for (i in 1:numOfComps){
    
    dist.all[[i]] <- c(dist.values[[i]],rep.values[[i]])
    feature.1[[i]] <- abs(((dist.feature[[i]] - mean(dist.all[[i]]))/sd(dist.all[[i]])) - ((rep.feature[[i]]- mean(dist.all[[i]]))/sd(dist.all[[i]])))
  }
  
  #feature.1 <- feature.1[feature.1 !="NaN"]
  #feature.1 <- feature.1[feature.1 !=0]
  
  #--- same for Collins/other set
  for (i in 1:numOfComps){
    dist.all.2[[i]] <- c(dist.values[[i]],rep2.values[[i]])
    
    feature.2[[i]] <- abs(((dist.feature.2[[i]] - mean(dist.all.2[[i]]))/sd(dist.all.2[[i]])) - ((rep2.feature[[i]]- mean(dist.all.2[[i]]))/sd(dist.all.2[[i]])))
  }
  
  #feature.2 <- feature.2[feature.2 !="NaN"]
  #feature.2 <- feature.2[feature.2 !=0]
  
  #------select highest no. of terms:  at the moment: everything above mean for set 
  
  mean.1 <- mean(feature.1)
  feature.1.red <- as.matrix(feature.1[feature.1 > (alpha*mean.1)])
  
  mean.2 <- mean(feature.2)
  feature.2.red <- as.matrix(feature.2[feature.2 > (alpha* mean.2)])
  
  values <- list()
  values[["features.1"]] <- feature.1.red
  values[["features.2"]] <- feature.2.red
  return(values)
  
}