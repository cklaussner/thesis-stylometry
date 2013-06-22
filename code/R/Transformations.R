library(psych)
# Matrix Transformations

#-- change dtm term weighting with log.

nV <- nV[,order(nV[1,],decreasing = TRUE)] # this is just ordering according to first row= 1. doc, maybe not represenatative 

#write.table(d,file="doc.txt",sep="\t", quote=FALSE)
#d2 <- dr[!dr %in% intersect]

#-----------
f <-  as.integer(rownames(feature.1.red))
colnames(s) <- c(1:numOfComps)
S.red <- S[,f]
#-----------

Vsize <- dim(nV)
numOfDocs <- Vsize[1] 
numOfTerms <- Vsize[2] 

n.relFreq <- matrix(0,nrow=numOfDocs, numOfTerms)
rownames(n.relFreq) <- rownames(nV)
colnames(n.relFreq) <- colnames(nV)
for (n in rownames(nV)){
  
  curr.Doc <- nV[n,]
  w.Token <- sum(curr.Doc) # sum over all tokens in doc
  w.Type <- length(curr.Doc[curr.Doc != 0]) 
  
  for(i in 1:length(curr.Doc)){
    
    curr.Val <- curr.Doc[i]
    
    rel.Freq <- (curr.Val +1)/ (w.Token+w.Type)
  
    n.relFreq[n,i] <- (rel.Freq)
  }
  
}
nV <- n.relFreq









-----------------------------------------
  
  
  hist.res <- diff$hist.res
cv <- diff$cv
sim <- diff$sim
feat <- diff$feat
D.feat <- diff$D.feat
O.feat <- diff$O.feat
D.feat.O <- diff$D.feat.O
O.feat.O <-  diff$O.feat.O
D.consist <- diff$D.consist
O.consist <- diff$O.consist
hist <- diff$hist
hist2 <- diff$hist2



save(hist.res,cv,sim,feat,D.feat,O.feat,
     D.feat.O,O.feat.O,D.consist,O.consist,hist,hist2, 
     file='DCTabata4999.rda')


-----------------------------------------------------


#-------------Evaluation

dickens.list <- names(diff$Dickens)[substr((names(diff$Dickens)),1,1) == "D"]
other.list <- names(diff$Dickens)[substr((names(diff$Dickens)),1,1) != "D"]

dsize <- length(dickens.list)
osize <- length(other.list)
results <- matrix(0, nrow = (dsize+2),ncol=3)
rownames(results) <- c(dickens.list,"mean","sum")
colnames(results) <- c("Dist.D.","Dist.C.","(Collins-Dickens)")
d <- diff[[1]]
c <- diff[[2]]

for (n in dickens.list){
  results[n,1] <- d[[n]]
  results[n,2] <- c[[n]]
  results[n,3] <- c[[n]]-d[[n]]

}
results[dsize+1,] <- c(mean(results[1:dsize,1]),mean(results[1:dsize,2]),mean(results[1:dsize,3]))
results[dsize+2,] <- c(sum(results[1:dsize,1]),sum(results[1:dsize,2]),sum(results[1:dsize,3]))

results.2 <- matrix(0, nrow = (osize+2),ncol=3)
rownames(results.2) <- c(other.list,"mean","sum")
colnames(results.2) <- c("Dist.D.","Dist.C.","(Dickens-Collins)")
d <- diff[[1]]
c <- diff[[2]]

for (n in other.list){
  
    results.2[n,1] <- d[[n]]
    results.2[n,2] <- c[[n]]
    results.2[n,3] <- d[[n]]-c[[n]]
  
}
results.2[osize+1,] <- c(mean(results.2[1:osize,1]),mean(results.2[1:osize,2]),mean(results.2[1:osize,3]))
results.2[osize+2,] <- c(sum(results.2[1:osize,1]),sum(results.2[1:osize,2]),sum(results.2[1:osize,3]))


matrix.RD <- as.matrix(rep(0,nrow=num.Docs,ncol=num.Docs))
for (i in 1:length(diff$dis.Matrix)){
  
  m <- data.frame(diff$dis.Matrix[i])
  matrix.RD <- matrix.RD +m
  
}

matrix.RD <- matrix.RD/length(diff$dis.Matrix)

#---plotting

nV.pca <- prcomp(nV)
summary(nV.pca)
nV.latent <- nV.pca$rotation
signif(sort(nV.latent[,1],decreasing=TRUE)[1:30],2) # 1 st component

pdf("file.pdf")
plot(nV.pca$x[,1:2], main = "Dickens vs. Collins", xlab= "PC1: +the +her +she +which +from -and -but -that -upon", ylab= "PC2: +her +you +she +had +your -and -the -their -they", type= "n")
points(nV.pca$x[1:55,1:2],pch="D",col="blue")
points(nV.pca$x[56:86,1:2],pch="C",col="green")
dev.off()


biplot(nV.pca, main = "Dickens vs. Collins",xlabs=c, cex= 0.7)
