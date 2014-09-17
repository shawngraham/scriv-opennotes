#library(devtools)
#create("~/githubstuff/scriv-opennotes/scriv-opennotes/another-project/Rdataanalysis")
#install.packages("mallet")
#setwd("~/githubstuff/scriv-opennotes/scriv-opennotes/another-project/Rdataanalysis")

#' this is adapted from Ben Marwick's Day of Archaeology 2013 analysis
#' this is for an example of using Mimno's wrapper for Mallet
#' we're using the sample data that comes bundeled when you download MALLET from
#' http://mallet.cs.umass.edu/download.php
#' we've assumed that, on Mac, you've put MALLET that you unzipped from Umass into a folder
#' under [user], ie "shawngraham/mallet-2.0.7"
#' on windows, use the full path "C:>\\mallet-2.0.7\\"
#' insert the path to your documents between the quotation marks
#' and windows users be sure to use \\ instead of a single \

#' we're assuming that you've already installed the mallet wrapper for R; if not, uncomment and run this line:
#' install.packages('mallet') 

#' if you are using Mavericks OS there could be a problem in installation - see chapter four for solution.

require(mallet)

#' import the documents from the folder
#' each document is here its own text file

documents <- mallet.read.dir("inst/extdata/") 

#' now grab the stop list
mallet.instances <- mallet.import(documents$id, documents$text, "inst/stoplists/en.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#' create topic trainer object
n.topics <- 30
topic.model <- MalletLDA(n.topics)

#' load documents
topic.model$loadDocuments(mallet.instances)

#' Get the vocabulary, and some statistics about word frequencies.
#' These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
#' Optimize hyperparameters every 20 iterations,
#' after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

#' Now train a model. Note that hyperparameter optimization is on, by default.
#' We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(200)

## NEW: run through a few iterations where we pick the best topic for each token,
## rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)  ##adap jockers wordcloud script to use this variable

# from http://www.cs.princeton.edu/~mimno/R/clustertrees.R
## transpose and normalize the doc topics
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "topics-docs.csv" ) 
## Get a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels
write.csv(topics.labels, "topics-labels.csv") 

### do word clouds of the topics
library(wordcloud)
for(i in 1:30){
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words[i,], 25)
  print(wordcloud(topic.top.words$words,
                  topic.top.words$weights,
                  c(4,.8), rot.per=0,
                  random.order=F))
}

