library(tm)  
library (plyr)


source("preprocessing/prepMat.R")
source("ica/rmvMean.R")
source("ica/svdMat.R")
source("ica/pcamat.R")
source("ica/ica.R")
source("ica/calcIC.R")
####"/home/carmen/Dropbox/Thesis/Data/text/SpecializedSet/"


args <- commandArgs(TRUE)
loc <- (args[1])  


################################## text preprocessing  #######################################

dtm <- prepMat(loc)   # dtm with tf weighting
spDtm <- removeSparseTerms(dtm, 0.4) # 2nd argument indication of sparsity in matrix 


######################################### ICA ####################################

# input is document term matrix with some kind of frequency weighting ####TODO experiment with different weighting schemes

#############   centering  ############

dtmCent <- rmvMean(spDtm)
mixedMean <- getFeatureMean()

############ whitening ############## 

pc <- svdMat(dtmCent)  # calculate new components with svd 
xwhiten <- getwhiteningMatrix()

#pc <-pcamat(dtmCent) # also works, but svd gives better approx.
############# ica ###################


ics <- ica(pc,xwhiten, 4)
icasig <- calcIC(spDtm,ics,mixedMean)

