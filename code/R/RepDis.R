# Representative-Distinctiveness Feature Selection

repDisComp <- function(S,noOfD,noOfC){   # imput is doc-comp matrix S, num of Dickens doc and Collins, assuming D. comes first

  prim.set <- S[1:noOfD, ]
  sec.set <- S[(noOfD+1):(noOfD+noOfC), ]
  
  Ssize <- dim(S)
  numOfComps <- Ssize[2] 

#-------- Representativeness: compare features within D.set - want lowest distance overall

rep.feature <- list()
rep.values <- list()

dist.feature <- list()
dist.values <- list()

for (i in 1:numOfComps){

#primary set
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
    sum.values <- c(sum.values,dist.dd)
  }
}
rep.values[[t]] <- sum.values
rep.feature[t] <- (2/ (abs(numOfP)^2 - abs(numOfP)))* sum(sum.values)


}










}

