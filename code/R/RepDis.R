# Representative-Distinctiveness Feature Selection

repDisComp <- function(S,noOfD,noOfC){   # imput is doc-comp matrix S, num of Dickens doc and Collins, assuming D. comes first

S <- abs(S)

D.set <- S[1:noOfD, ]
C.set <- S[(noOfD+1):(noOfD+noOfC), ]

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
    
    d.1 <- log(D.set[[j,i]])    
    if((D.set[[j,i]])==0) d.1 <- 0
    
     for (jj in j+1:(numOfD-j)){
       d.2 <- log(D.set[[jj,i]])
       if((D.set[[jj,i]])==0) d.2 <- 0
       
       dist.dd <- abs(d.1-d.2)
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
     d.1 <- log(D.set[[j,i]])
     if((D.set[[j,i]])== 0) d.1 <- 0
     for (jj in 1:numOfCol){
        c.1 <- log(C.set[[jj,i]])
        if((C.set[[jj,i]])== 0) c.1 <- 0
        dist.dc <- abs(d.1 -c.1)
        
        sum.valuesDC <- c(sum.valuesDC,dist.dc)
     }
  }
  dist.values[[i]] <- sum.valuesDC
  dist.feature[i] <- (2/ ((abs(numOfD))* (abs(numOfCol) - abs(numOfD))))* sum(sum.valuesDC)

}



#------ Feature comparison

dist.all <- list()
feature.comp <- matrix(0,nrow=numOfComps,ncol=1)
for (i in 1:numOfComps){
  dist.all[[i]] <- c(dist.values[[i]],rep.values[[i]])
  
  feature.comp[i,] <- abs((dist.feature[[i]] - mean(dist.all[[i]]))/sd(dist.all[[i]])) - ((rep.feature[[i]]- mean(dist.all[[i]]))/sd(dist.all[[i]]))
  
}

#------select highest no. of terms:  at the moment: everything above mean for set 

 mean.comp <- mean(feature.comp)
 comp.red <- c()
 for (i in 1:numOfComps){
   if(feature.comp[i] > mean.comp){
     comp.red <- c(comp.red,i)
   }
 }


return(comp.red)
 
}



