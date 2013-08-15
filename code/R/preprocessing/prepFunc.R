library(tm)  
#library (plyr)

# set parameters
#sparsity = "1.0"

# create document-term matrix with prepocessing 

gsub2<- function(myPattern, myReplacement, myCorpus, fixed=FALSE,ignore.case=FALSE){
  for (i in 1:length(myCorpus)){
    for (j in 1:length(myPattern)){
      myCorpus[[i]]<- gsub(myPattern[j],myReplacement[j], myCorpus[[i]], fixed=TRUE)
    }
  }
  return(myCorpus)
}


###FINAl sets
#DickensCollins: "/home/carmen/Dropbox/Thesis/Data/Final-sets/DickensCollins-simple/"
#Worldset: "/home/carmen/Dropbox/Thesis/Data/Final-sets/World/"
#Testdoc: "home/carmen/Dropbox/Thesis/Data/Final-sets/Test-docs/"

prepare <- function(loc) {

  print("...creating Text corpus...")
 
 (docCorpus <- Corpus(DirSource(loc),readerControl = list(language = "en")))   # create corpus from source

  
  gsub2<- function(myPattern, myReplacement, myCorpus, fixed=FALSE,ignore.case=FALSE){
    for (i in 1:length(myCorpus)){
      for (j in 1:length(myPattern)){
        myCorpus[[i]]<- gsub(myPattern[j],myReplacement[j], myCorpus[[i]], fixed=TRUE)
      }
    }
    return(myCorpus)
  }
  
  
  docCorpus <- gsub2("-"," ",docCorpus,fixed=TRUE,ignore.case=TRUE)
  #docCorpus <- gsub2("\""," ",docCorpus,fixed=TRUE,ignore.case=TRUE)
  #docCorpus <- gsub2(":"," ",docCorpus,fixed=TRUE,ignore.case=TRUE)
  
  
  
  print("...prepocessing, - to lower case, removing Numbers and Punctuation...")
      
    #textCorpus <- tm_map(textCorpus, stripWhitespace)  # seems to have some unusal behaviour
   
    textCorpus <- tm_map(docCorpus,removePunctuation)
    textCorpus <- tm_map(textCorpus,removeNumbers)
    textCorpus  <- tm_map(textCorpus, tolower)


print("...creating Document-by-Term Matrix...")

  #dtm <- DocumentTermMatrix(textCorpus, control = list(weighting = weightTfIdf))

docTermM <- DocumentTermMatrix(textCorpus)
return(docTermM)
}


rmvSparsity <- function(dtm,sparsity){

print("removing sparse terms")
 spDtm <- removeSparseTerms(dtm, sparsity)
 
 return(spDtm)
}



