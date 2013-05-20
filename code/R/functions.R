
library(stats)
library(cluster)
library(fpc)
# Functions more independent of application


check.data <- function(dataset,noOfD,noOfO){

if ((substr(rownames(dataset)[1],1,1) == "D")){
  print("Dickens is first")
  prim.set <- dataset[1:noOfD, ]
  sec.set <- dataset[(noOfD+1):(noOfD+noOfO), ]
}else{
  print("Comparison set is first")
  sec.set <- dataset[1:noOfO, ]
  prim.set <- dataset[(noOfO+1):(noOfD+noOf0), ]
}

return(rbind(prim.set, sec.set)) # have dataset in right order: Dickens-nonDickens
}



featureConsistency <- function(feature.list){
  
  consist <- list()
  results <- matrix(0, nrow = length(feature.list)+2,ncol=2)
  rownames(results) <- c(c(names(feature.list)),"mean", "std.")
  colnames(results) <- c("list length","list after inter.")
  
  sum.feat <- c()
  for (i in names(feature.list)){
    
    sum.feat <- c(sum.feat,length(feature.list[[i]]))
    print(sum.feat)
    results[i, 1] <- length(feature.list[[i]])
  }
  results["mean", 1] <- mean(sum.feat)
  results["std.", 1] <- sd(sum.feat)
  
  if (length(feature.list)>1){
  intersect <- rownames(feature.list[[1]]) 
  results[1, 2] <- length(feature.list[[1]])
  sum.int <- length(feature.list[[1]])
  for (i in 2:length(feature.list)){
    intersect <- intersect(intersect,rownames(feature.list[[i]]))
    results[i, 2] <- length(intersect)
    sum.int <- c(sum.int,length(intersect))
  }
  results["mean", 2] <- mean(sum.int)
  results["std.", 2] <- sd(sum.int)
  consist[["intersect"]] <- intersect
}
  consist[["results"]] <- results
   
}









# computes combination of various dissim. Matrices for a list of features 
dissim.Matrix <- function(input,RD.features){

  m <- as.matrix(log(input[,c(rownames(RD.features))]))
  RD.matrix <- as.matrix((dist(m, method="manhattan", diag=TRUE, upper=TRUE))/length(RD.features))# account for no of features compared

return(RD.matrix)

}

# compute corrected Rand ind. for clustering based on dissim. matrix 


getCorrRand <- function(dM, Metric, noOfD, noOfO){
  
clust <- agnes(dM, diss = TRUE, metric = Metric) # cluster according to dissim. 
branches <- cutree(clust,2)  # get separation for two clusters

d <- c(rep(1,noOfD))
nd <- c(rep(2,noOfO))
c <- c(d,nd)

corrected.Rand <- (cluster.stats(dM,branches,c))$corrected.rand

return(corrected.Rand)
}

hist.diff <- function(test.set,RD.features){

termsize.train <- sum(RD.features)
hist.train <- RD.features/termsize.train

# extract keywords from test set vector + histogram
test.vec <- as.matrix(test.set[rownames(RD.features),]) # retain only RD.features 
termsize.test <- sum(test.vec)
hist.test <- test.vec/termsize.test

abs.diff <- (sum(abs(hist.train - hist.test)))/length(RD.features) # calculate absolute difference


return(abs.diff)
}

# sum up results of cross-validation

cv.results <- function(D.diff,O.diff,clust.eval,clust.eval.2){
  
cv <- list()
  
dickens.list <- names(D.diff)[substr((names(D.diff)),1,1) == "D"] # get Dickens elements tested 
nonDickens.list <- names(D.diff)[substr((names(D.diff)),1,1) != "D"] # get nonDickens elements tested 

dsize <- length(dickens.list)
osize <- length(nonDickens.list)
if (dsize != 0){ # this is for testing on Dickens documents

results <- matrix(0, nrow = (dsize+2),ncol=4)
rownames(results) <- c(dickens.list,"mean","sum")

colnames(results) <- c("Dist.D.","Dist.nD.","(Dist.nD-Dist.D)", "cor.Rand:Sim. Clust.")
d <- D.diff
c <- O.diff

for (n in dickens.list){
  results[n,1] <- d[[n]]
  results[n,2] <- c[[n]]
  results[n,3] <- c[[n]]-d[[n]]
  results[n,4] <- clust.eval[[n]] # note corrected Rand result
}
results[dsize+1,] <- c(mean(results[1:dsize,1]),mean(results[1:dsize,2]),mean(results[1:dsize,3]),mean(results[1:dsize,4]) )
results[dsize+2,] <- c(sum(results[1:dsize,1]),sum(results[1:dsize,2]),sum(results[1:dsize,3]),sum(results[1:dsize,4]))

#t-test

#D.T <- t.test(results[1:dsize,1],results[1:dsize,2],alternative="greater",var.equal=T)

#cv[["D-t"]] <- D.T
cv[["Dickens"]] <- results

}
if (osize != 0){ # this is for testing on nonDickens documents  - other perspective
  
results.2 <- matrix(0, nrow = (osize+2),ncol=4)
rownames(results.2) <- c(nonDickens.list,"mean","sum")
colnames(results.2) <- c("Dist.D.","Dist.nD.","(Dist.D-Dist.nD)","cor.Rand:Sim. Clust.")
d <- D.diff
c <- O.diff

for (n in other.list){
  
  results.2[n,1] <- d[[n]]
  results.2[n,2] <- c[[n]]
  results.2[n,3] <- d[[n]]-c[[n]]
  results.2[n,4] <- clust.eval.2[[n]]
}
results.2[osize+1,] <- c(mean(results.2[1:osize,1]),mean(results.2[1:osize,2]),mean(results.2[1:osize,3]),mean(results.2[1:osize,4]))
results.2[osize+2,] <- c(sum(results.2[1:osize,1]),sum(results.2[1:osize,2]),sum(results.2[1:osize,3]),sum(results.2[1:osize,4]))


nD.T <- t.test(results.2[1:osize,1],results.2[1:osize,2],alternative="greater",var.equal=T)


cv[["nonDickens"]] <- results.2
cv[["nD-t"]] <-    nD.T
}

return(cv)
}








# only for R. and D. output



distribute.RD <- function(dataset, noOfD, noOfO){
  
  term.lists <- list()
  prim.set <- dataset[1:noOfD, ]
  sec.set <- dataset[(noOfD+1):(noOfD+noOfO), ]
  
# check frequencies of terms

#freq.list <- matrix(0,nrow= length(colnames(dataset)), ncol=2)
#rownames(freq.list) <- colnames(dataset)
#colnames(freq.list) <- c("D","nD")
d.terms <- c()
nd.terms <- c()
for (l in colnames(prim.set)){
  
  D <- sum(prim.set[,l])/noOfD
  nD <- sum(sec.set[,l])/noOfO
  if (D > nD){
    #freq.list[l,1] <- sum(prim.set[,l])/noOfD
    d.terms <- c(d.terms,l)
  }else{
    #freq.list[l,2] <- sum(sec.set[,l])/noOfO
    nd.terms <- c(nd.terms,l)
  }
}

  term.lists[["D"]] <- d.terms
  term.lists[["nD"]] <- nd.terms

  return(term.lists)
}

