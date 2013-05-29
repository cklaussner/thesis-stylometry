library(tm)  
#library (plyr)

# set parameters
#sparsity = "1.0"

# create document-term matrix with prepocessing 

prepare <- function(loc) {

  print("...creating Text corpus...")
 
 (docCorpus <- Corpus(DirSource(loc),readerControl = list(language = "en")))   # create corpus from source


  print("...prepocessing, - to lower case, removing Numbers and Punctuation...")
      
    #textCorpus <- tm_map(textCorpus, stripWhitespace)  # seems to have some unusal behaviour 
    textCorpus  <- tm_map(docCorpus, tolower)

    textCorpus <- tm_map(textCorpus,removePunctuation)
    textCorpus <- tm_map(textCorpus,removeNumbers)



print("...creating Document-by-Term Matrix...")

docTermM <- DocumentTermMatrix(textCorpus)
return(docTermM)
}


rmvSparsity <- function(dtm,sparsity){

print("removing sparse terms")
 spDtm <- removeSparseTerms(dtm, sparsity)
 
 return(spDtm)
}



