install.packages("mallet")
library(mallet)
cc <- read.csv("data/citizensconnect.csv")
n <- 25
topic.model <- MalletLDA(num.topics=n)
mallet.instances <- mallet.import(id.array = row.names(cc), text.array = as.character(cc$description), stoplist.file = "data/stop_311.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
word.freqs$term.freq.n <- word.freqs$term.freq/sum(word.freqs$term.freq)
word.freqs$doc.freq.n <- word.freqs$doc.freq/sum(word.freqs$doc.freq)
topic.model$train(500)
topic.model$maximize(10)
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
top.words <- data.frame()
for (i in 1:nrow(topic.words)) {
  tmp <- mallet.top.words(topic.model, topic.words[i,], num.top.words = 10)
  tmp$topic <- i
  top.words <- rbind( top.words, tmp)
}
rm(i);rm(tmp)







#install.packages("wordcloud")
library(wordcloud)
pal <- brewer.pal(4,"Paired")

for (i in 1: n) {
  tmp <- top.words[top.words$topic==i,]
  wordcloud(tmp$words,freq = tmp$weights,random.order=FALSE, rot.per = 0,colors=pal, scale = c(3,1))
  text(x=0.5, y=0.1, paste("Topic",i))
  #quartz.save(paste("out/",i,".pdf", sep = ""), type = "pdf")#, width = 5, height = 4)
}


