
cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.
require(RCurl)    #Downloading Package
require(XML)
library(RCurl)     #Using package in this program
library(XML)


#Set the working directory to your workspace
setwd("/Users/mohsennabian/Dropbox/Spring 2106/Visualization/Proj1")

theurl<-"file:///Users/mohsennabian/Dropbox/Spring%202106/Visualization/Proj1/search-history.html"
webpage <- getURL(theurl)
# convert the page into a line-by-line format rather than a single string
tc <- textConnection(webpage)
webpage <- readLines(tc) #webpage is now a vector of string each elament is a line of string
close(tc)
pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)  #pagetree is now in html format and parseable with xpath syntax.
searches<- unlist(xpathApply(pagetree,"//*/td",xmlValue))


searches<-searches[str_detect(searches, "201")]
searches<-searches[str_detect(searches, "T")]
searches<-searches[str_detect(searches, "Z")]
searches
library(lubridate)
y<-c()
m<-c()
d<-c()
hours<-c()
for (i in c(1:length(searches)))
{
  tm<-ymd_hms(searches[i])
  y[i]<-year(tm)
  m[i]<-month(tm)
  d[i]<-day(tm)
  hours[i]<-hour(tm)
}
hours<-hours-5
hours<-hours%%24
df<-data.frame(hours,y,m,d)
df2<-df[!(df[,2]==2012),]   #Removing 2012 uses
df3<-df2[!(df2[,2]==2016),]  #Removing 2016 and 2012


library(ggplot2)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               cxv f




g10<-ggplot(data=df3,aes(x = hours, group = as.factor(y)))+geom_density(aes(fill =as.factor(y), color =as.factor(y)), alpha = 0.5)
g10<-g10+xlab("Hours")+ylab("Youtube Visit Density")
g10<-g10+theme(axis.text=element_text(size=15),axis.title=element_text(size=20,face="bold"))
g10<-g10+guides(fill=FALSE)
g10<-g10+guides(fill=guide_legend(title="Year"))  
g10


df2$m<-as.factor(df2$m)

df2$m<-factor(df2$m, levels = rev(levels(df2$m)))

#########################   Month

g2<-ggplot(data=df2, aes(df2$m,fill=as.factor(df2$y)))+geom_bar()+coord_flip()
g2<-g2+scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12"),labels=c("Januray","Febreury", "March","April","May","June","July","August","September","October","November","December"))
g2<-g2+xlab("Month")+ylab("Youtube Visit")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20,face="bold"))
g2<-g2+guides(fill=guide_legend(title="Year"))                
g2

#######################

bp<- ggplot(df2, aes(x="", y=y, fill=y))
bp<-bp+geom_bar(width = 1, stat = "identity")
pie<-bp + coord_polar("y", start=0)
pie


library(plotrix)
months<-table(df$m)
class(months)
months[1]+2

lbls <- c("January", "Febreury", "March", "April", "May","June", "July", "August", "September", "October","November", "December")
pie3D(months,labels =lbls ,explode=0.1,
      main="Pie Chart of Countries ")











########################
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

theurl<-"file:///Users/mohsennabian/Dropbox/Spring%202106/Visualization/Proj1/2015.txt"
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
findFreqTerms(seuss.tdm, lowfreq=300)
# which words are associated with "brains"?
findAssocs(seuss.tdm, 'brains', 0.30)
# Word Cloud
m <- as.matrix(seuss.tdm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
ss<-as.character(d$word)
ss
d<-d[!str_detect(ss,"\\?"),]
head(d)
#pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq,scale=c(8,.2),min.freq=10,max.words=Inf, random.order=FALSE, rot.per=.15, colors='blue')

















