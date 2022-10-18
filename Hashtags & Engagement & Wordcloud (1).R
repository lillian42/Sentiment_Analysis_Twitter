setwd("E:/JHU Spring/Social Media Analytics/Final Project")
library(rtweet)
northman_final <- read_twitter_csv("northman_final_dataset.csv")
View(northman_final)
# Turn every letter into lower case
northman_final$hashtags=tolower(northman_final$hashtags)
#sort the hashtags
hash=sort(table(unlist(northman_final$hashtags)), decreasing = TRUE)
View(hash)
#how many hashtags contain "the northman" regardless of upper or lowercase
hashtagkeyword=grepl("[Th]e[Nn]orthman", northman_final$hashtags)
table(hashtagkeyword)
#propotion
prop.table(table(hashtagkeyword))
#how many tweets contain the key wold "the northman"
keyword1=grepl("[Th]e[Nn]orthman", northman_final$text)
prop.table(table(keyword1))

#how many tweets contain the key wold "the northman"
keyword1=grepl("[Th]e[Nn]orthman", northman_final$text)
table(keyword1)

#some other keywold
keyword2=grepl("[Mm]ovie", northman_final$text)
table(keyword2)

keyword3=grepl("[Ss]karsgard", northman_final$text)
table(keyword3)

keyword4=grepl("[Gg]reat", northman_final$text)
table(keyword4)


#engagement analysis#
library(diffusion)
library(anytime)
#modify the date format
northman_final$datetime = anytime(northman_final$created_at)
northman_final$date = format(northman_final$datetime, '%m%d')
#The engagement change over time, the peak is April 23
ts_plot(northman_final,by ="days")
ts_plot(northman_final,by ="4hours")

library("dplyr")
# select the date column 
date= northman_final %>% select(date)
View(date)
# a pivot table summarizing number of tweets by each day 
date %>%
  group_by(date) %>%
  summarize(count_by_day = n())
# make the pivot table a matrix 
counttrend = as.matrix(date %>%
            group_by(date) %>%
            summarize(count_by_day = n())
          )
View(counttrend)  
day= c(1:16)
counttrend= cbind(day,counttrend)
# a scatter plot of number of tweets change over time
plot(count_by_day ~ day, data=counttrend)
counttrend=as.data.frame(counttrend)

# calculate cumulative number of tweet for bass model 
counttrend$cumulative = cumsum(counttrend$count_by_day)
counttrend$count_by_day=as.numeric(counttrend$count_by_day)
# apply bass model 
fit=diffusion(counttrend$count_by_day,type = "bass")
par(mar=c(3,3,3,3))
fit
plot(fit,cumulative = FALSE)
plot(fit,cumulative = TRUE)


#key word, wordcloud
library(data.table)
library(textcat)

library(tm)
library(textstem)

library(RColorBrewer)
library(wordcloud)

library(lda)
library(topicmodels)

#filter dataframe by date 0420
northman0420 <- filter(northman_final,date== "0420")

language=textcat(northman0420$text)
sort(table(language),decreasing=T)
# english reviews
northman0420=northman0420[language=="english",]
n0420=northman0420$text
View(n0420)

docs0420 = VCorpus(VectorSource(n0420))
# Preprocess the data:
# Transform to lower case
docs0420 <-tm_map(docs0420,content_transformer(tolower))

#remove punctuation
docs0420 = tm_map(docs0420, removePunctuation)

#Strip digits
docs0420 = tm_map(docs0420, removeNumbers)

#remove stopwords
docs0420 = tm_map(docs0420, removeWords, stopwords("english"))

#remove whitespace
docs0420 = tm_map(docs0420, stripWhitespace)

# lemmatize the words 
# e.g. ran and run will both be converted to run
docs0420 = tm_map(docs0420, content_transformer(lemmatize_strings))

#convert data into document term matrix format
docsmatrix0420 = DocumentTermMatrix(docs0420)


# Generate wordcloud for all the documents
# Calculate the frequency of each word
docsmatrix0420=as.matrix(docsmatrix0420)
WD0420 = sort(colSums(docsmatrix0420),decreasing=TRUE)

# Transform the data to fit with the wordcloud package
WD0420=data.table(words=names(WD0420),freq=WD0420)

par(mar=c(1,1,1,1))
wordcloud(words = WD0420$words, freq = WD0420$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


#filter dataframe by date 0421
northman0421 <- filter(northman_final,date== "0421")
language=textcat(northman0421$text)
sort(table(language),decreasing=T)
northman0421=northman0421[language=="english",]
n0421=northman0421$text
docs0421 = VCorpus(VectorSource(n0421))
docs0421 <-tm_map(docs0421,content_transformer(tolower))
docs0421 = tm_map(docs0421, removePunctuation)
docs0421 = tm_map(docs0421, removeNumbers)
docs0421 = tm_map(docs0421, removeWords, stopwords("english"))
docs0421 = tm_map(docs0421, stripWhitespace)
docs0421 = tm_map(docs0421, content_transformer(lemmatize_strings))
docsmatrix0421 = DocumentTermMatrix(docs0421)
docsmatrix0421=as.matrix(docsmatrix0421)
WD0421 = sort(colSums(docsmatrix0421),decreasing=TRUE)
WD0421=data.table(words=names(WD0421),freq=WD0421)
par(mar=c(1,1,1,1))
wordcloud(words = WD0421$words, freq = WD0421$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0422
northman0422 <- filter(northman_final,date== "0422")
language=textcat(northman0422$text)
sort(table(language),decreasing=T)
northman0422=northman0422[language=="english",]
n0422=northman0422$text
docs0422 = VCorpus(VectorSource(n0422))
docs0422 <-tm_map(docs0422,content_transformer(tolower))
docs0422 = tm_map(docs0422, removePunctuation)
docs0422 = tm_map(docs0422, removeNumbers)
docs0422 = tm_map(docs0422, removeWords, stopwords("english"))
docs0422 = tm_map(docs0422, stripWhitespace)
docs0422 = tm_map(docs0422, content_transformer(lemmatize_strings))
docsmatrix0422 = DocumentTermMatrix(docs0422)
docsmatrix0422=as.matrix(docsmatrix0422)
WD0422 = sort(colSums(docsmatrix0422),decreasing=TRUE)
WD0422=data.table(words=names(WD0422),freq=WD0422)
par(mar=c(1,1,1,1))
wordcloud(words = WD0422$words, freq = WD0422$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0423
northman0423 <- filter(northman_final,date== "0423")
language=textcat(northman0423$text)
sort(table(language),decreasing=T)
northman0423=northman0423[language=="english",]
n0423=northman0423$text
docs0423 = VCorpus(VectorSource(n0423))
docs0423 <-tm_map(docs0423,content_transformer(tolower))
docs0423 = tm_map(docs0423, removePunctuation)
docs0423 = tm_map(docs0423, removeNumbers)
docs0423 = tm_map(docs0423, removeWords, stopwords("english"))
docs0423 = tm_map(docs0423, stripWhitespace)
docs0423 = tm_map(docs0423, content_transformer(lemmatize_strings))
docsmatrix0423 = DocumentTermMatrix(docs0423)
docsmatrix0423=as.matrix(docsmatrix0423)
WD0423 = sort(colSums(docsmatrix0423),decreasing=TRUE)
WD0423=data.table(words=names(WD0423),freq=WD0423)
par(mar=c(1,1,1,1))
wordcloud(words = WD0423$words, freq = WD0423$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0424
northman0424 <- filter(northman_final,date== "0424")
language=textcat(northman0424$text)
sort(table(language),decreasing=T)
northman0424=northman0424[language=="english",]
n0424=northman0424$text
docs0424 = VCorpus(VectorSource(n0424))
docs0424 <-tm_map(docs0424,content_transformer(tolower))
docs0424 = tm_map(docs0424, removePunctuation)
docs0424 = tm_map(docs0424, removeNumbers)
docs0424 = tm_map(docs0424, removeWords, stopwords("english"))
docs0424 = tm_map(docs0424, stripWhitespace)
docs0424 = tm_map(docs0424, content_transformer(lemmatize_strings))
docsmatrix0424 = DocumentTermMatrix(docs0424)
docsmatrix0424=as.matrix(docsmatrix0424)
WD0424 = sort(colSums(docsmatrix0424),decreasing=TRUE)
WD0424=data.table(words=names(WD0424),freq=WD0424)
par(mar=c(1,1,1,1))
wordcloud(words = WD0424$words, freq = WD0424$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0425
northman0425 <- filter(northman_final,date== "0425")
language=textcat(northman0425$text)
sort(table(language),decreasing=T)
northman0425=northman0425[language=="english",]
n0425=northman0425$text
docs0425 = VCorpus(VectorSource(n0425))
docs0425 <-tm_map(docs0425,content_transformer(tolower))
docs0425 = tm_map(docs0425, removePunctuation)
docs0425 = tm_map(docs0425, removeNumbers)
docs0425 = tm_map(docs0425, removeWords, stopwords("english"))
docs0425 = tm_map(docs0425, stripWhitespace)
docs0425 = tm_map(docs0425, content_transformer(lemmatize_strings))
docsmatrix0425 = DocumentTermMatrix(docs0425)
docsmatrix0425=as.matrix(docsmatrix0425)
WD0425 = sort(colSums(docsmatrix0425),decreasing=TRUE)
WD0425=data.table(words=names(WD0425),freq=WD0425)
par(mar=c(1,1,1,1))
wordcloud(words = WD0425$words, freq = WD0425$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0426
northman0426 <- filter(northman_final,date== "0426")
language=textcat(northman0426$text)
sort(table(language),decreasing=T)
northman0426=northman0426[language=="english",]
n0426=northman0426$text
docs0426 = VCorpus(VectorSource(n0426))
docs0426 <-tm_map(docs0426,content_transformer(tolower))
docs0426 = tm_map(docs0426, removePunctuation)
docs0426 = tm_map(docs0426, removeNumbers)
docs0426 = tm_map(docs0426, removeWords, stopwords("english"))
docs0426 = tm_map(docs0426, stripWhitespace)
docs0426 = tm_map(docs0426, content_transformer(lemmatize_strings))
docsmatrix0426 = DocumentTermMatrix(docs0426)
docsmatrix0426=as.matrix(docsmatrix0426)
WD0426 = sort(colSums(docsmatrix0426),decreasing=TRUE)
WD0426=data.table(words=names(WD0426),freq=WD0426)
par(mar=c(1,1,1,1))
wordcloud(words = WD0426$words, freq = WD0426$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0427
northman0427 <- filter(northman_final,date== "0427")
language=textcat(northman0427$text)
sort(table(language),decreasing=T)
northman0427=northman0427[language=="english",]
n0427=northman0427$text
docs0427 = VCorpus(VectorSource(n0427))
docs0427 <-tm_map(docs0427,content_transformer(tolower))
docs0427 = tm_map(docs0427, removePunctuation)
docs0427 = tm_map(docs0427, removeNumbers)
docs0427 = tm_map(docs0427, removeWords, stopwords("english"))
docs0427 = tm_map(docs0427, stripWhitespace)
docs0427 = tm_map(docs0427, content_transformer(lemmatize_strings))
docsmatrix0427 = DocumentTermMatrix(docs0427)
docsmatrix0427=as.matrix(docsmatrix0427)
WD0427 = sort(colSums(docsmatrix0427),decreasing=TRUE)
WD0427=data.table(words=names(WD0427),freq=WD0427)
par(mar=c(1,1,1,1))
wordcloud(words = WD0427$words, freq = WD0427$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0428
northman0428 <- filter(northman_final,date== "0428")
language=textcat(northman0428$text)
sort(table(language),decreasing=T)
northman0428=northman0428[language=="english",]
n0428=northman0428$text
docs0428 = VCorpus(VectorSource(n0428))
docs0428 <-tm_map(docs0428,content_transformer(tolower))
docs0428 = tm_map(docs0428, removePunctuation)
docs0428 = tm_map(docs0428, removeNumbers)
docs0428 = tm_map(docs0428, removeWords, stopwords("english"))
docs0428 = tm_map(docs0428, stripWhitespace)
docs0428 = tm_map(docs0428, content_transformer(lemmatize_strings))
docsmatrix0428 = DocumentTermMatrix(docs0428)
docsmatrix0428=as.matrix(docsmatrix0428)
WD0428 = sort(colSums(docsmatrix0428),decreasing=TRUE)
WD0428=data.table(words=names(WD0428),freq=WD0428)
par(mar=c(1,1,1,1))
wordcloud(words = WD0428$words, freq = WD0428$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0429
northman0429 <- filter(northman_final,date== "0429")
language=textcat(northman0429$text)
sort(table(language),decreasing=T)
northman0429=northman0429[language=="english",]
n0429=northman0429$text
docs0429 = VCorpus(VectorSource(n0429))
docs0429 <-tm_map(docs0429,content_transformer(tolower))
docs0429 = tm_map(docs0429, removePunctuation)
docs0429 = tm_map(docs0429, removeNumbers)
docs0429 = tm_map(docs0429, removeWords, stopwords("english"))
docs0429 = tm_map(docs0429, stripWhitespace)
docs0429 = tm_map(docs0429, content_transformer(lemmatize_strings))
docsmatrix0429 = DocumentTermMatrix(docs0429)
docsmatrix0429=as.matrix(docsmatrix0429)
WD0429 = sort(colSums(docsmatrix0429),decreasing=TRUE)
WD0429=data.table(words=names(WD0429),freq=WD0429)
par(mar=c(1,1,1,1))
wordcloud(words = WD0429$words, freq = WD0429$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0430
northman0430 <- filter(northman_final,date== "0430")
language=textcat(northman0430$text)
sort(table(language),decreasing=T)
northman0430=northman0430[language=="english",]
n0430=northman0430$text
docs0430 = VCorpus(VectorSource(n0430))
docs0430 <-tm_map(docs0430,content_transformer(tolower))
docs0430 = tm_map(docs0430, removePunctuation)
docs0430 = tm_map(docs0430, removeNumbers)
docs0430 = tm_map(docs0430, removeWords, stopwords("english"))
docs0430 = tm_map(docs0430, stripWhitespace)
docs0430 = tm_map(docs0430, content_transformer(lemmatize_strings))
docsmatrix0430 = DocumentTermMatrix(docs0430)
docsmatrix0430=as.matrix(docsmatrix0430)
WD0430 = sort(colSums(docsmatrix0430),decreasing=TRUE)
WD0430=data.table(words=names(WD0430),freq=WD0430)
par(mar=c(1,1,1,1))
wordcloud(words = WD0430$words, freq = WD0430$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0501
northman0501 <- filter(northman_final,date== "0501")
language=textcat(northman0501$text)
sort(table(language),decreasing=T)
northman0501=northman0501[language=="english",]
n0501=northman0501$text
docs0501 = VCorpus(VectorSource(n0501))
docs0501 <-tm_map(docs0501,content_transformer(tolower))
docs0501 = tm_map(docs0501, removePunctuation)
docs0501 = tm_map(docs0501, removeNumbers)
docs0501 = tm_map(docs0501, removeWords, stopwords("english"))
docs0501 = tm_map(docs0501, stripWhitespace)
docs0501 = tm_map(docs0501, content_transformer(lemmatize_strings))
docsmatrix0501 = DocumentTermMatrix(docs0501)
docsmatrix0501=as.matrix(docsmatrix0501)
WD0501 = sort(colSums(docsmatrix0501),decreasing=TRUE)
WD0501=data.table(words=names(WD0501),freq=WD0501)
par(mar=c(1,1,1,1))
wordcloud(words = WD0501$words, freq = WD0501$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0502
northman0502 <- filter(northman_final,date== "0502")
language=textcat(northman0502$text)
sort(table(language),decreasing=T)
northman0502=northman0502[language=="english",]
n0502=northman0502$text
docs0502 = VCorpus(VectorSource(n0502))
docs0502 <-tm_map(docs0502,content_transformer(tolower))
docs0502 = tm_map(docs0502, removePunctuation)
docs0502 = tm_map(docs0502, removeNumbers)
docs0502 = tm_map(docs0502, removeWords, stopwords("english"))
docs0502 = tm_map(docs0502, stripWhitespace)
docs0502 = tm_map(docs0502, content_transformer(lemmatize_strings))
docsmatrix0502 = DocumentTermMatrix(docs0502)
docsmatrix0502=as.matrix(docsmatrix0502)
WD0502 = sort(colSums(docsmatrix0502),decreasing=TRUE)
WD0502=data.table(words=names(WD0502),freq=WD0502)
par(mar=c(1,1,1,1))
wordcloud(words = WD0502$words, freq = WD0502$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0503
northman0503 <- filter(northman_final,date== "0503")
language=textcat(northman0503$text)
sort(table(language),decreasing=T)
northman0503=northman0503[language=="english",]
n0503=northman0503$text
docs0503 = VCorpus(VectorSource(n0503))
docs0503 <-tm_map(docs0503,content_transformer(tolower))
docs0503 = tm_map(docs0503, removePunctuation)
docs0503 = tm_map(docs0503, removeNumbers)
docs0503 = tm_map(docs0503, removeWords, stopwords("english"))
docs0503 = tm_map(docs0503, stripWhitespace)
docs0503 = tm_map(docs0503, content_transformer(lemmatize_strings))
docsmatrix0503 = DocumentTermMatrix(docs0503)
docsmatrix0503=as.matrix(docsmatrix0503)
WD0503 = sort(colSums(docsmatrix0503),decreasing=TRUE)
WD0503=data.table(words=names(WD0503),freq=WD0503)
par(mar=c(1,1,1,1))
wordcloud(words = WD0503$words, freq = WD0503$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0504
northman0504 <- filter(northman_final,date== "0504")
language=textcat(northman0504$text)
sort(table(language),decreasing=T)
northman0504=northman0504[language=="english",]
n0504=northman0504$text
docs0504 = VCorpus(VectorSource(n0504))
docs0504 <-tm_map(docs0504,content_transformer(tolower))
docs0504 = tm_map(docs0504, removePunctuation)
docs0504 = tm_map(docs0504, removeNumbers)
docs0504 = tm_map(docs0504, removeWords, stopwords("english"))
docs0504 = tm_map(docs0504, stripWhitespace)
docs0504 = tm_map(docs0504, content_transformer(lemmatize_strings))
docsmatrix0504 = DocumentTermMatrix(docs0504)
docsmatrix0504=as.matrix(docsmatrix0504)
WD0504 = sort(colSums(docsmatrix0504),decreasing=TRUE)
WD0504=data.table(words=names(WD0504),freq=WD0504)
par(mar=c(1,1,1,1))
wordcloud(words = WD0504$words, freq = WD0504$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

#filter dataframe by date 0505
northman0505 <- filter(northman_final,date== "0505")
language=textcat(northman0505$text)
sort(table(language),decreasing=T)
northman0505=northman0505[language=="english",]
n0505=northman0505$text
docs0505 = VCorpus(VectorSource(n0505))
docs0505 <-tm_map(docs0505,content_transformer(tolower))
docs0505 = tm_map(docs0505, removePunctuation)
docs0505 = tm_map(docs0505, removeNumbers)
docs0505 = tm_map(docs0505, removeWords, stopwords("english"))
docs0505 = tm_map(docs0505, stripWhitespace)
docs0505 = tm_map(docs0505, content_transformer(lemmatize_strings))
docsmatrix0505 = DocumentTermMatrix(docs0505)
docsmatrix0505=as.matrix(docsmatrix0505)
WD0505 = sort(colSums(docsmatrix0505),decreasing=TRUE)
WD0505=data.table(words=names(WD0505),freq=WD0505)
par(mar=c(1,1,1,1))
wordcloud(words = WD0505$words, freq = WD0505$freq, 
          scale=c(2,0.5),
          min.freq = 300,
          max.words=4000, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))
