

d1 <- log(nV[,"upon"])
dM <- dist(d1, method="manhattan", diag=TRUE, upper=TRUE)
a <- agnes(dM, diss = TRUE, metric = "complete")
ad <- cutree(a,2)

length(c) <-86
c[1:55] <- 1
c[56:86] <- 2
cs <- cluster.stats(dM,ad,c)

cs$corrected.rand




D.sim[[remove.doc]] <- matrix.RD/length(RD.features)


if (length(dis.Matrix) > 1){
  for (i in 2:length(dis.Matrix)){
    
    m <- as.matrix(dis.Matrix[[i]])
    matrix.RD <- (matrix.RD + m)
    
  }
  
  boxplot(nonsmokers,smokers,ylab="Scores on Digit Span Task",names=c("nonsmokers","smokers"),main="Digit Span Performance by\n Smoking Status")
  