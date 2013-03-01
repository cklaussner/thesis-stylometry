
library(tm)  
library (plyr)

source("preprocessing/prepMat.R")


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
newVectors <- dtmCent[1] # get new centered vectors
mixedmean <- dtmCent[2] # get mean deducted for later

############ whitening ############## 

vals <- pcamat(newVectors)
eigenvals <- vals[1]
eigenvecs <- vals[2]

#pcas= whitenv(spDtm,eigenvals, eigenvecs)  # calculate new components

############# ica ###################


















