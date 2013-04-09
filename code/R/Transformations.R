
# Matrix Transformations

#-- change dtm term weighting with log.

n <- nV
nl <- log(n)
nl[nl==-Inf] <- 0
nl[nl==Inf] <- 0

nV <- nl




m <- matrix(0, nrow = length(diff[[1]]),ncol=3)
rownames(m) <- names(diff[[1]])
colnames(m) <- c("Dist.D.","Dist.C.","(Collins-Dickens)")
for (n in names(diff[[1]])){
  
  d <- diff[[1]]
  c <- diff[[2]]
  val1 <- d[[n]]
  val2 <- c[[n]]
  m[n,1] <- val1
  m[n,2] <- val2
  m[n,3] <- val2-val1
}


m <- matrix(0, nrow = length(diff[[1]]),ncol=3)
rownames(m) <- names(diff[[1]])
colnames(m) <- c("Dist.D.","Dist.C.","(Dickens-Collins)")
for (n in names(diff[[1]])){
  
  d <- diff[[1]]
  c <- diff[[2]]
  val1 <- d[[n]]
  val2 <- c[[n]]
  m[n,1] <- val1
  m[n,2] <- val2
  m[n,3] <- val1-val2
}