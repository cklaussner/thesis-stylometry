#library(tm)  
#library (plyr)

# set parameters
#sparsity = "1.0"

# create document-term matrix with prepocessing 

corpus <- function(corpusLoc) {

  print("...creating Text corpus...")
 
 (docCorpus <- Corpus(DirSource(loc),readerControl = list(language = "en")))   # create corpus from source

 return(docCorpus)
 }
 
 
 prep <- function(textCorpus){
 
      print("...prepocessing, - to lower case, removing Numbers and Punctuation...")
      
    #textCorpus <- tm_map(textCorpus, stripWhitespace)  # seems to have some unusal behaviour 
    textCorpus  <- tm_map(textCorpus, tolower)

    textCorpus <- tm_map(textCorpus,removePunctuation)
    textCorpus <- tm_map(textCorpus,removeNumbers)
    
    return(textCorpus)
}


dtm <- function(textCorpus){

print("...creating Document-by-Term Matrix...")

docTermM <- DocumentTermMatrix(textCorpus)
return(docTermM)
}


rmvSparsity <- function(dtm,sparsity){

print("removing sparse terms")
 spDtm <- removeSparseTerms(dtm, sparsity)
 
 return(spDtm)
}



