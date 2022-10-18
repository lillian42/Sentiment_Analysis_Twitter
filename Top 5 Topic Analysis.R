# Set working directory
setwd("~/R-Work/Social Media Analytics/FINAL PROJECT")


# Install and import data preprocessing packages
library(rtweet)
library(data.table)
library(textcat)
library(tm)
library(textstem)
library(RColorBrewer)
library(wordcloud)
library(lda)
library(topicmodels)

doa <- read_twitter_csv("nm_official_account.csv")
temp <- doa$text
docs = VCorpus(VectorSource(temp)) #transform sentences into corpus 


# Preprocess the data:
# Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))

#remove punctuation
docs = tm_map(docs, removePunctuation)

#Strip digits
docs = tm_map(docs, removeNumbers)

#remove stopwords
docs = tm_map(docs, removeWords, stopwords("english"))

#remove whitespace
docs = tm_map(docs, stripWhitespace)

# lemmatize the words 
# e.g. ran and run will both be converted to run
docs = tm_map(docs, content_transformer(lemmatize_strings))

#Calculate the document term matrix
DTM = DocumentTermMatrix(docs)

# Generate wordcloud for all the documents
# Calculate the frequency of each word
DTMMATRIX=as.matrix(DTM)
WD = sort(colSums(DTMMATRIX),decreasing=TRUE)
WD

# Transform the data to fit with the wordcloud package
WCLOUD=data.table(words=names(WD),freq=WD)

# Plot out the wordcloud
par(mar=c(1,1,1,1))
wordcloud(words = WCLOUD$words, freq = WCLOUD$freq, 
          scale=c(4,1),
          min.freq = 2,
          max.words=500, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

# Apply topic models to identify common themes
# Transform the data to fit with the topic model package
input = dtm2ldaformat(DTM)

# set the random seed so results are replicable
set.seed(12345)

# select model parameters to be 2 topics,  select parameters
K=3 # set the number of topics
N=15 # set the number of iterations
result = lda.collapsed.gibbs.sampler(
  input$documents,
  K,            # The number of topics.
  input$vocab,
  N,           # The number of iterations.
  1/K,          # The Dirichlet hyper parameter for topic proportion
  0.1,          # The Dirichlet hyper parameter for topic multinomial
  compute.log.likelihood=TRUE)

plot(result$log.likelihoods[1,],type="o")

# Get the top five words in each cluster. 
# Top words are the characteristics of the relevant topic.
TOPIC = top.topic.words(result$topics, 5, by.score=TRUE)
TOPIC

# Count number of topic 1 and topic 2 keywords for each sentence 
result$document_sums # row: one sentence; column: one topic
T1=t(result$document_sums)

# Calculate the topic assignment for each sentence
topicproportion=T1/rowSums(T1)
topicproportion
colMeans(topicproportion)




