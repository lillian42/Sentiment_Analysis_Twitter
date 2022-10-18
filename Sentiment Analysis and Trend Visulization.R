#### Sentiment Analysis####


setwd("/Users/Social Media Analytics/Group Project")
northman=fread("nm_final.csv")
summary(northman$created_at)
fb=fread("fb_final.csv")
summary(fb$created_at)
sonic=fread("sonic_final.csv")
summary(sonic$created_at)

library(sentimentr) # Sentiment analysis package
library(textcat) # Select English reviews
library(data.table)

words = lexicon::hash_sentiment_jockers_rinker[T] #sentiment word dictionary

#sentiment function
sentiment_calculation<-function(df){
  # Select English tweets
  language=textcat(df$text)
  eng=df[language=="english",] # eng: all English tweets
  
  # Sentiment score for sentences
  temp=eng$text
  sentence=get_sentences(temp)
  sentence_score=sentiment(sentence)
  sentence_score=as.data.table(sentence_score)
  
  # Sentiment score for each text
  text_score=sentence_score[,
                            list(
                              text.sentiment=mean(sentiment)
                            ),
                            by=list(element_id)]
  
  eng=cbind(eng,text_score) 
  
  
  # Analyze sentiment score
  summary_sentiment_score=summary(eng$text.sentiment) 
  max20=eng[order(eng$text.sentiment),"text"][1:20]
  min20=eng[order(-eng$text.sentiment),"text"][1:20]

  # Trend Analysis
  eng$date = as.Date(eng$created_at, format='%m/%d/%Y')
  day_score=eng[,
                list(
                  meansentiment=mean(text.sentiment)
                ),
                by=date]
  day_score=day_score[order(day_score$date),]

  out<-list(max=max20,min=min20,summary_sentiment_score=summary_sentiment_score,day_score=day_score)
  return(out)
}

summary_northman<-sentiment_calculation(northman) # Mean 0.10425
nm_score=summary_northman$summary_sentiment_score
nm_max=summary_northman$max
nm_min=summary_northman$min  # 20 tweets of lowest sentiment score
summary_fb<-sentiment_calculation(fb) # Mean 0.08426
fb_score=summary_fb$summary_sentiment_score
summary_sonic<-sentiment_calculation(sonic) # Mean  0.1471
sonic_score=summary_sonic$summary_sentiment_score

###################################

#plot three films
summary_northman$day_score$meansentiment
day=1:23
combine_sentiment=cbind(day,summary_northman$day_score$meansentiment,
      summary_fb$day_score$meansentiment,summary_sonic$day_score$meansentiment)
colnames(combine_sentiment)=c("day","TheNorthman","FantasticBeast","Sonic")
combine_sentiment=as.data.frame(combine_sentiment)

library(reshape2)
library(ggplot2)

mydata <- melt(combine_sentiment,id="day")
colnames(mydata)<-c("Day","Film","SentimentScore")
ggplot(data = mydata,aes(x=Day,y=SentimentScore,group = Film,color=Film,shape=Film))+
  geom_point(size=2)+
  geom_line(size=1.5)+
  xlab("Day")+
  ylab("Sentiment Score")+
  theme_bw() + # Remove grey background 
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), # Above theme: keep the axis border and remove grid line
        legend.position = c(.075,.915), # legend position to top left
        legend.box.background = element_rect(color="black"))+ # legend boder
        scale_x_continuous(limits = c(1,23),breaks = seq(1,23,1)) #axis scale
        


