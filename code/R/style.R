library(tm)  
library (plyr)
library(far) 

source("preprocessing/prepMat.R")
source("ica/rmvMean.R")
source("ica/svdMat.R")
source("ica/pcamat.R")
####"/home/carmen/Dropbox/Thesis/Data/text/SpecializedSet/"


args <- commandArgs(TRUE)
loc <- (args[1])  


################################## text preprocessing  #######################################

dtm <- prepMat(loc)   # dtm with tf weighting
spDtm <- removeSparseTerms(dtm, 0.8) # 2nd argument indication of sparsity in matrix 


######################################### ICA ####################################

# input is document term matrix with some kind of frequency weighting ####TODO experiment with different weighting schemes

#############   centering  ############

dtmCent <- rmvMean(spDtm)
#newVectors <- dtmCent[1] # get new centered vectors
#mixedmean <- dtmCent[2] # get mean deducted for later

############ whitening ############## 

pc <- svdMat(dtmCent)  # calculate new components with svd 
xwhiten <- getwhiteningMatrix()
#pc <-pcamat(dtmCent)
############# ica ###################


#ic <- ica(pc,xwhiten, noOfIC)

