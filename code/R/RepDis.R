library(fields)
# Representative-Distinctiveness Feature Selection

S <- abs(S)
#Ssize <- dim(S)
#numOfDocs <- Ssize[1] 
#numOfComps <- Ssize[2] 


D.set <- S[1:55, ]
C.set <- S[55:86, ]

Dsize <- dim(D.set)
numOfD <- Dsize[1] 
numOfComps <- Dsize[2] 

Csize <- dim(C.set)
numOfCol <- Csize[1] 

#--------normalize feature weight ????


#-------- Representativeness: compare features within D.set - want lowest distance overall

rep.feature <- list()
rep.values <- list()

for (i in 1:numOfComps){
  
  sum.values <- c()
  for (j in 1:(numOfD-1)){
    
    d.1 <- log(D.set[j,i])    # change log ?
     for (jj in j+1:(numOfD-j)){
       d.2 <- log(D.set[jj,i])
       dist.dd <- rdist(d.1,d.2) # change to whatever other way of comp. distance
       sum.values <- c(sum.values,dist.dd)
       
      }
  }
  rep.values[[i]] <- sum.values
  
  rep.feature[i] <- 2/ (abs(numOfD)^2 - abs(numOfD))* sum(sum.values)
}

#------------Distinctiveness: compare Dickens and Collins set

dist.feature <- list()
dist.values <- list()
for (i in 1:numOfComps){
  sum.valuesDC <- c()
  for (j in 1:numOfD){
     d.1 <- log(D.set[j,i])
     
     for (jj in 1:numOfCol){
        c.1 <- log(C.set[jj,i])
    
        dist.dc <- rdist(d.1,c.1) # change to whatever other way of comp. distance
        sum.valuesDC <- c(sum.valuesDC,dist.dc)
     }
  }
  dist.values[[i]] <- sum.valuesDC
  dist.feature[i] <- (2/ ((abs(numOfD))* (abs(numOfCol) - abs(numOfD))))* sum(sum.valuesDC)

}



#------ Feature comparison
dist.all <- list()
feature.comp <- list()
for (i in 1:numOfComps){
  dist.all[[i]] <- c(dist.values[[i]],rep.values[i])
  
  feat.comp[i] <- ((dist.feature[i] - mean(dist.all[i]))/sd(dist.all[i])) - ((rep.feature[i]- mean(dist.all[i]))/sd(dist.all[i]))
  
}




