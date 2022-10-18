##### Set working directory
setwd("~/R-Work/Social Media Analytics/FINAL PROJECT")


##### Install and import data preprocessing packages
library(rtweet)
library(data.table)
library(textcat)
library(tm)
library(textstem)
library(RColorBrewer)
library(wordcloud)
library(lda)
library(topicmodels)

northman_final <- read_twitter_csv("nm_final.csv")


##### Split the dataset to conduct day-by-day analysis

# Modify the date format
library(diffusion)
library(anytime)
northman_final$datetime = anytime(northman_final$created_at)
northman_final$date = format(northman_final$datetime, '%m%d')

# Split by date
list_ds <- split(northman_final, northman_final$date) # A list of everyday's datasets

##### Create a fuction with the analysis process then apply to each datasets
analyze <- function(x1){
  temp = x1$text
  docs = VCorpus(VectorSource(temp)) #transform sentences into corpus 
  
  ### Preprocess the data:
  # Transform to lower case
  docs <-tm_map(docs,content_transformer(tolower))
  # Remove punctuation
  docs = tm_map(docs, removePunctuation)
  # Strip digits
  docs = tm_map(docs, removeNumbers)
  # Remove stopwords
  docs = tm_map(docs, removeWords, stopwords("english"))
  # Remove whitespace
  docs = tm_map(docs, stripWhitespace)
  # Lemmatize the words
  docs = tm_map(docs, content_transformer(lemmatize_strings))
  # Calculate the document term matrix
  DTM = DocumentTermMatrix(docs)
  
  ### Generste word cloud
  # Generate wordcloud for all the documents
  # Calculate the frequency of each word
  DTMMATRIX=as.matrix(DTM)
  WD = sort(colSums(DTMMATRIX),decreasing=TRUE)
  # Transform the data to fit with the wordcloud package
  WCLOUD=data.table(words=names(WD),freq=WD)
  
  par(mar=c(1,1,1,1))
  wordcloud(words = WCLOUD$words, freq = WCLOUD$freq, 
            scale=c(5,1),
            min.freq = 2,
            max.words=200, 
            random.order=FALSE,
            rot.per=0.1, 
            colors=brewer.pal(8, "Dark2"))
  
  ### Apply topic models to identify common themes
  # Transform the data to fit with the topic model package
  input = dtm2ldaformat(DTM)
  
  ### Find top 2 topics in each day
  # Set the random seed so results are replicable
  set.seed(12345)
  # Select model parameters to be 2 topics,  select parameters
  K=2 # Set the number of topics
  N=10 # Set the number of iterations
  result = lda.collapsed.gibbs.sampler(
    input$documents,
    K,            # The number of topics.
    input$vocab,
    N,           # The number of iterations.
    1/K,          # The Dirichlet hyper parameter for topic proportion
    0.1,          # The Dirichlet hyper parameter for topic multinomial
    compute.log.likelihood=TRUE)
  
  plot(result$log.likelihoods[1,],type="o")
  # Get the top 10 words in each cluster. 
  # Top words are the characteristics of the relevant topic.
  TOPIC = top.topic.words(result$topics, 10, by.score=TRUE)
  TOPIC
}

# Apply lapply function
lapply(list_ds, analyze)


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="~/R-Work/Social Media Analytics/FINAL PROJECT/nm_everyday_wc_plots")

