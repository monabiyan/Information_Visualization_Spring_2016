
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
setwd("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Proj3")

  txt<-"/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Proj3/good.txt"
  searches <- readLines(txt)
  searches
  
  
 
  seuss.corpus <- Corpus(DataframeSource(data.frame(searches)))
  seuss.corpus
  inspect(seuss.corpus)
  seuss.corpus[1]

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
  wordcloud(d$word,d$freq,min.freq=4,max.words=Inf, random.order=TRUE, colors='navy')

  #########################  NAME  ###############################
  
  
  
  
  