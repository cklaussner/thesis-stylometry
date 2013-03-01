#library(tm)  
#library (plyr)

source("preprocessing/prepFunc.R") # import preprocessing functions
 
prepMat <-function(loc){

corp <- corpus(loc);  # create corpus
prepText <- prep(corp)
dtm <- dtm(prepText)
return(dtm)
}