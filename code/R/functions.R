library(mclust)
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
  prim.set <- dataset[(noOfO+1):(noOfD+noOfO), ]
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
    
    results[i, 1] <- length(feature.list[[i]])
     
  }
 results["mean", 1] <- round(mean(sum.feat))
results["std.", 1] <- round(sd(sum.feat))
  
  if (length(feature.list)>1){
  intersect <- rownames(feature.list[[1]]) 
  results[1, 2] <- length(feature.list[[1]])
  sum.int <- length(feature.list[[1]])
  for (i in 2:length(feature.list)){
    intersect <- intersect(intersect,rownames(feature.list[[i]]))
    results[i, 2] <- length(intersect)
    sum.int <- c(sum.int,length(intersect))
  }
  results["mean", 2] <- round(mean(sum.int))
  results["std.", 2] <- round(sd(sum.int))
  consist[["intersect"]] <- intersect
}
  consist[["results"]] <- results
  return(consist)
   
}


featureConsistency.2 <- function(feature.list){
  
  consist <- list()
  results <- matrix(0, nrow = length(feature.list)+2,ncol=2)
  rownames(results) <- c(c(1:length(feature.list)),"mean", "std.")
  colnames(results) <- c("list length","list after inter.")
  
  sum.feat <- c()
  for (i in 1:length(feature.list)){
    
    sum.feat <- c(sum.feat,length(feature.list[[i]]))
    
    results[i, 1] <- length(feature.list[[i]])
    
  }
  results["mean", 1] <- round(mean(sum.feat))
  results["std.", 1] <- round(sd(sum.feat))
  
  if (length(feature.list)>1){
    intersect <- rownames(feature.list[[1]]) 
    results[1, 2] <- length(feature.list[[1]])
    sum.int <- length(feature.list[[1]])
    for (i in 2:length(feature.list)){
      intersect <- intersect(intersect,rownames(feature.list[[i]]))
      results[i, 2] <- length(intersect)
      sum.int <- c(sum.int,length(intersect))
    }
    results["mean", 2] <- round(mean(sum.int))
    results["std.", 2] <- round(sd(sum.int))
    consist[["intersect"]] <- intersect
  }
  consist[["results"]] <- results
  return(consist)
  
}





# computes combination of various dissim. Matrices for a list of features 
dissim.Matrix <- function(input,RD.features){

  m <- as.matrix(input[,RD.features])
  RD.matrix <- as.matrix((dist(m, method="manhattan", diag=TRUE, upper=TRUE)))# account for no of features compared

return(RD.matrix)

}

# compute corrected Rand ind. for clustering based on dissim. matrix 


getAdjRand <- function(dM, Metric, noOfD, noOfO){
  
clust <- agnes(dM, diss = TRUE, metric = Metric) # cluster according to dissim. 
branches <- cutree(clust,2)  # get separation for two clusters

d <- c(rep(1,noOfD))
nd <- c(rep(2,noOfO))
c <- c(d,nd)

adj.Rand <- adjustedRandIndex(branches, c)

return(adj.Rand)
}

hist.diff <- function(test.set,RD.features){

termsize.train <- sum(abs(RD.features))
hist.train <- RD.features/termsize.train

# extract keywords from test set vector + histogram
test.vec <- as.matrix(test.set[rownames(RD.features),]) # retain only RD.features 
termsize.test <- sum(abs(test.vec))
hist.test <- test.vec/termsize.test

#abs.diff <- (sum(abs(hist.train - hist.test)))/length(RD.features) # calculate absolute difference
abs.diff <- (mean(abs(hist.train - hist.test)))

return(abs.diff)
}


hist.diffC <- function(test.set,RD.features){
  
  termsize.train <- sum(abs(RD.features))
  hist.train <- RD.features/termsize.train
  
  # extract keywords from test set vector + histogram
  test.vec <- as.matrix(test.set[rownames(RD.features),]) # retain only RD.features 
  termsize.test <- sum(abs(test.vec))
  hist.test <- test.vec/termsize.test
  
  abs.diffC <-abs(hist.train - hist.test) # calculate absole difference
  
  
  return(abs.diffC)
}


# sum up results of cross-validation

cv.results <- function(D.diff,O.diff,clust.eval,hist,hist2){
  
cv <- list()
  
dickens.list <- names(D.diff)[substr((names(D.diff)),1,1) == "D"] # get Dickens elements tested 
nonDickens.list <- names(D.diff)[substr((names(D.diff)),1,1) != "D"] # get nonDickens elements tested 

dsize <- length(dickens.list)
osize <- length(nonDickens.list)
if (dsize != 0){ # this is for testing on Dickens documents

results <- matrix(0, nrow = (dsize+2),ncol=7)
rownames(results) <- c(dickens.list,"mean","sum")

colnames(results) <- c("Dist.D.","Dist.nD.","(Dist.nD-Dist.D)", "adjust.Rand","p-val.","conf-int-high","conf-int-low")
d <- D.diff
c <- O.diff

for (n in dickens.list){
  
  ttest <- (t.test(hist2[[n]],hist[[n]],alternative="greater"))
  results[n,5] <- ttest$p.value # we assume nonDickens to have larger difference 
  results[n,6] <- ttest$conf.int[1]
  results[n,7] <- ttest$conf.int[2]
  results[n,1] <- d[[n]]
  results[n,2] <- c[[n]]
  results[n,3] <- c[[n]]-d[[n]]
  for (nt in names(clust.eval)){
    if (nt %in% dickens.list){
      results[nt,4] <- clust.eval[[nt]] # note corrected Rand result for iteration always marked on first document
    }}
  
}
results[dsize+1,1:4] <- c(mean(results[1:dsize,1]),mean(results[1:dsize,2]),mean(results[1:dsize,3]),mean(results[1:dsize,4]) )
results[dsize+2,1:4] <- c(sum(results[1:dsize,1]),sum(results[1:dsize,2]),sum(results[1:dsize,3]),sum(results[1:dsize,4]))

#t-test

#D.T <- t.test(results[1:dsize,1],results[1:dsize,2],alternative="greater")

#cv[["D-t"]] <- D.T
cv[["Dickens"]] <- results

}
if (osize != 0){ # this is for testing on nonDickens documents  - other perspective
  
results.2 <- matrix(0, nrow = (osize+2),ncol=4)
rownames(results.2) <- c(nonDickens.list,"mean","sum")
colnames(results.2) <- c("Dist.D.","Dist.nD.","(Dist.D-Dist.nD)","adjust.Rand")
d <- D.diff
c <- O.diff

for (n in nonDickens.list){
  ttest <- (t.test(hist2[[n]],hist[[n]],alternative="greater"))
  results.2[n,5] <- ttest$p.value # we assume Dickens to have larger difference 
  results.2[n,6] <- ttest$conf.int[1]
  results.2[n,7] <- ttest$conf.int[2]
  results.2[n,1] <- d[[n]]
  results.2[n,2] <- c[[n]]
  results.2[n,3] <- d[[n]]-c[[n]]
  for (nt in names(clust.eval)){
    if (nt %in% nonDickens.list){
      results[nt,4] <- clust.eval[[nt]] # note corrected Rand result for iteration always marked on first document
    }}
  
}
results.2[osize+1,1:4] <- c(mean(results.2[1:osize,1]),mean(results.2[1:osize,2]),mean(results.2[1:osize,3]),mean(results.2[1:osize,4]))
results.2[osize+2,1:4] <- c(sum(results.2[1:osize,1]),sum(results.2[1:osize,2]),sum(results.2[1:osize,3]),sum(results.2[1:osize,4]))


#nD.T <- t.test(results.2[1:osize,1],results.2[1:osize,2],alternative="greater",var.equal=T)


cv[["nonDickens"]] <- results.2

}

return(cv)
}




cv.results.3 <- function(D.diff,O.diff,clust.eval, hist,hist2){
  
  cv <- list()
  
  dickens.list <- names(D.diff)[substr((names(D.diff)),1,1) == "D"] # get Dickens elements tested 
  nonDickens.list <- names(D.diff)[substr((names(D.diff)),1,1) != "D"] # get nonDickens elements tested 
  
  dsize <- length(dickens.list)
  osize <- length(nonDickens.list)
  if (dsize != 0){ # this is for testing on Dickens documents
    
  results <- matrix(0, nrow = dsize+4,ncol=7)
  rownames(results) <- c(dickens.list,"mean","sum","sd","SE")
  
  colnames(results) <- c("Dist.D.","Dist.nD.","(Dist.nD-Dist.D)", "adjust.Rand", "p-val.","conf-int-low","conf-int-high")
  d <- D.diff
  c <- O.diff
  
  for (n in dickens.list){
    
    results[n,1] <- d[[n]]
    results[n,2] <- c[[n]]
    results[n,3] <- c[[n]]-d[[n]]
    ttest <- (t.test(hist2[[n]],hist[[n]],alternative="greater"))
    results[n,5] <- ttest$p.value # we assume nonDickens to have larger difference 
    results[n,6] <- ttest$conf.int[1]
    results[n,7] <- ttest$conf.int[2]
      for (nt in names(clust.eval)){
        if (nt %in% dickens.list){
       results[nt,4] <- clust.eval[[nt]] # note corrected Rand result for iteration always marked on first document
      }}
           ttest.whole <-(t.test(results[1:dsize,2],results[1:dsize,1],alternative="greater"))
           results[dsize+1,] <- c(mean(results[1:dsize,1]),mean(results[1:dsize,2]),mean(results[1:dsize,3]),mean(results[1:dsize,4][results[1:dsize,4]!=0]),ttest.whole$p.value,ttest.whole$conf.int[1],ttest.whole$conf.int[2])
           results[dsize+2,1:4] <- c(sum(results[1:dsize,1]),sum(results[1:dsize,2]),sum(results[1:dsize,3]),sum(results[1:dsize,4]))
           results[dsize+3,1:4]  <- c(sd(results[1:dsize,1]),sd(results[1:dsize,2]),sd(results[1:dsize,3]),sd(results[1:dsize,4]))
           results[dsize+4,1:4]  <- c(sd(results[1:dsize,1]/sqrt(dsize)),sd(results[1:dsize,2]/sqrt(dsize)),sd(results[1:dsize,3]/sqrt(dsize)),sd(results[1:dsize,4]/sqrt(dsize)))
  
  }
  cv[["Dickens"]] <- results
} 
  
  
  
  
  if (osize != 0){ # this is for testing on nonDickens documents
#     
#     
#     
#     
    results.2 <- matrix(0, nrow = osize+4,ncol=7)
    rownames(results.2) <- c(nonDickens.list,"mean","sum","sd","SE")
#     
     colnames(results.2) <- c("Dist.D.","Dist.nD.","(Dist.nD-Dist.D)", "adjust.Rand", "p-val.","conf-int-low","conf-int-high")
     d <- D.diff
     c <- O.diff
     
     for (n in nonDickens.list){
       
       results.2[n,1] <- d[[n]]
       results.2[n,2] <- c[[n]]
       results.2[n,3] <- c[[n]]-d[[n]]
         ttest <- (t.test(hist[[n]],hist2[[n]],alternative="greater"))
       results.2[n,5] <- ttest$p.value # we assume nonDickens to have larger difference 
       results.2[n,6] <- ttest$conf.int[1]
       results.2[n,7] <- ttest$conf.int[2]
       for (nt in names(clust.eval)){
         if (nt %in% nonDickens.list){
           results.2[nt,4] <- clust.eval[[nt]] # note corrected Rand result for iteration always marked on first document
         }}
       ttest.whole <-(t.test(results.2[1:osize,1],results.2[1:osize,2],alternative="greater"))
       results.2[osize+1,] <- c(mean(results.2[1:osize,1]),mean(results.2[1:osize,2]),mean(results.2[1:osize,3]),mean(results.2[1:osize,4][results.2[1:osize,4]!=0]),ttest.whole$p.value,ttest.whole$conf.int[1],ttest.whole$conf.int[2])
      results.2[osize+2,1:4] <- c(sum(results.2[1:osize,1]),sum(results.2[1:osize,2]),sum(results.2[1:osize,3]),sum(results.2[1:osize,4]))
       results.2[osize+3,1:4]  <- c(sd(results.2[1:osize,1]),sd(results.2[1:osize,2]),sd(results.2[1:osize,3]),sd(results.2[1:osize,4]))
       results.2[osize+4,1:4]  <- c(sd(results.2[1:osize,1]/sqrt(osize)),sd(results[1:osize,2]/sqrt(osize)),sd(results.2[1:osize,3]/sqrt(osize)),sd(results.2[1:osize,4]/sqrt(osize)))
       
    }
    cv[["nonDickens"]] <- results.2
  }
  
 
  
  return(cv)
}




# only for R. and D. output

distribute.RD <- function(dataset, noOfD, noOfO){
  
  term.lists <- list()
  prim.set <- dataset[1:noOfD, ]
  sec.set <- dataset[(noOfD+1):(noOfD+noOfO), ]
  
# check frequencies of terms

freq.list <- matrix(0,nrow= length(colnames(dataset)), ncol=2)
rownames(freq.list) <- colnames(dataset)
colnames(freq.list) <- c("D","nD")
d.terms <- c()
nd.terms <- c()
 
for (l in colnames(prim.set)){
  
  D <- sum(prim.set[,l])/sum(prim.set)
 nD <- sum(sec.set[,l])/sum(sec.set)
  
  freq.list[l,1] <- D
  freq.list[l,2] <- nD
    
  if (D > nD){

    d.terms <- c(d.terms,l)
  }else{
    nd.terms <- c(nd.terms,l)
  }
}
  
  term.lists[["D"]] <- d.terms
  term.lists[["nD"]] <- nd.terms
  term.lists[["freq.list"]] <- as.matrix(freq.list[order(freq.list[,1],decreasing = TRUE ),])
  
  return(term.lists)
}

