
cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.
require(RCurl)    #Downloading Package
require(XML)
library(RCurl)     #Using package in this program
library(XML)
require(stringr)
library(stringr)
library(tm)
library(RTextTools)
library(wordcloud)



#Set the working directory to your workspace
setwd("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Proj1")

  theurl<-"file:///Users/mohsennabian/Dropbox/Spring%202106/Visualization/Proj1/search-history.html"
  webpage <- getURL(theurl)
  # convert the page into a line-by-line format rather than a single string
  tc <- textConnection(webpage)
  webpage <- readLines(tc) #webpage is now a vector of string each elament is a line of string
  close(tc)
  pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)  #pagetree is now in html format and parseable with xpath syntax.
  searches<- unlist(xpathApply(pagetree,"//*/a[@*]",xmlValue))
  str_detect(searches, "Ù")
  searches<-searches[!(str_detect(searches, "Ù"))]
  searches
  
 
  seuss.corpus <- Corpus(DataframeSource(data.frame(searches)))
  seuss.corpus
  inspect(seuss.corpus)
  seuss.corpus[1]
  writeLines(as.character(seuss.corpus[1]))
  writeLines(as.character(seuss.corpus[1:3]))
  # Eliminating Extra Whitespace
  seuss.clean<-tm_map(seuss.corpus, stripWhitespace)
  # stemDocument
  seuss.clean.stem<-tm_map(seuss.clean, stemDocument)
  writeLines(as.character(seuss.clean.stem[1]))
  # "You have brains in your head."
  # Convert to Lower Case
  seuss.clean.lc <- tm_map(seuss.clean, content_transformer(tolower))
  writeLines(as.character(seuss.clean.lc[1]))
  # Remove Stopwords
  seuss.clean <- tm_map(seuss.clean.lc, removeWords, stopwords("english"))

  writeLines(as.character(seuss.clean.lc[1]))
  # Building a Document-Term Matrix
  seuss.tdm <- TermDocumentMatrix(seuss.clean, control = list(minWordLength = 1))
  seuss.tdm
  inspect(seuss.tdm[11:33,1:33])
  # Operations on Term-Document Matrices
  # Frequent Terms and Associations
  findFreqTerms(seuss.tdm, lowfreq=3)
  # which words are associated with "brains"?
  findAssocs(seuss.tdm, 'brains', 0.30)
  # Word Cloud
  m <- as.matrix(seuss.tdm)
  # calculate the frequency of words
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  d <- data.frame(word=myNames, freq=v)
  
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(d$word,d$freq,scale=c(8,.2),min.freq=15,max.words=Inf, random.order=FALSE, rot.per=.15, colors='navy')

  #########################  NAME  ###############################
  
  
  
  
  