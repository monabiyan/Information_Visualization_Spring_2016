# text analysis with the mallet tool for extracting topic models using Latent Dirichlet Allocation (LDA)
# for background, code & tutorials go to http://mallet.cs.umass.edu/

#install.packages("mallet")
library(mallet)

# load our sample text corpus: a selection of citizen complaint messages submitted via smartphone
cc <- read.csv("data/complaints.csv")

# initiate topic model trainer, with n topics (this value can be tweaked based on result)
n <- 25
topic.model <- MalletLDA(num.topics=n) 

## Load document corpus. We could also pass in the filename of a saved instance list file that we build from the command-line tools.
mallet.instances <- mallet.import(id.array = row.names(cc), text.array = as.character(cc$description), stoplist.file = "data/stop_311.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
##  These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
word.freqs$term.freq.n <- word.freqs$term.freq/sum(word.freqs$term.freq)
word.freqs$doc.freq.n <- word.freqs$doc.freq/sum(word.freqs$doc.freq)

# now train the topic models with 200 iterations
topic.model$train(500)

## run through a few iterations where we pick the best topic for each token
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

## create a top word list
top.words <- data.frame()
for (i in 1:nrow(topic.words)) {
  tmp <- mallet.top.words(topic.model, topic.words[i,], num.top.words = 10)
  tmp$topic <- i
  top.words <- rbind( top.words, tmp)
}
rm(i);rm(tmp)

#visualize word clouds
install.packages("wordcloud")
library(wordcloud)
pal <- brewer.pal(4,"Paired")

# plot each recognized topic as a wordcloud
# use back buttons to browse (or uncomment save command to save as pdf)
for (i in 1: n) {
  tmp <- top.words[top.words$topic==i,]
  wordcloud(tmp$words,freq = tmp$weights,random.order=FALSE, rot.per = 0,colors=pal, scale = c(3,1))
  text(x=0.5, y=0.1, paste("Topic",i))
  #quartz.save(paste("out/",i,".pdf", sep = ""), type = "pdf")#, width = 5, height = 4)
}

