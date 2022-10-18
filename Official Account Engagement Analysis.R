# Is the word-of-mouth effect strong or not?
# How does word-of-mouth affect the  popularity of the movie?
# Is the innovator’s effect strong or not?

### Load data
ds <- read_twitter_csv("northman_final_dataset.csv")
library(rtweet)
library(ggplot2)
library(dplyr)

# Find out number of tweets from the film's official account
official_account <- get_timelines("@TheNorthmanFilm",n=500)
write_as_csv(official_account, "nm_official_account.csv")
# 326 tweets in total


### Trend analysis: find out the number of tweets over time ()
# Times Series Analysis:plot out the total #of tweets on a unit time(by xxx)
ts_plot(official_account,by = "weeks")
ts_plot(official_account,by = "days")
ts_plot(official_account,by = "hours")

# @TheNorthmanFilm's social media activities
# Original tweets
# Retweets: broadcast messages from other accounts 
# Quote Tweets: Add own comments to messages from other accounts
# Private replies to some users: do not show up in the followers' feeds
# Self replies to 3M itself: show up in the followers' feeds

# Analysis on @TheNorthmanFilm's original messages
original = official_account[official_account$is_retweet==0&official_account$is_quote==0
                            &official_account$is_private==0
                            &official_account$is_self_reply==0,]


# Analysis for retweets
summary(official_account$is_retweet)
sum(official_account$is_retweet)
sum(official_account$is_retweet)/nrow(official_account)
retweet = official_account[official_account$is_retweet==1,]

# Analysis for quote tweet
summary(official_account$is_quote)
sum(official_account$is_quote)/nrow(official_account)
quote = official_account[official_account$is_quote==1,]

# Analysis for all replies (private replies and self replies)
official_account$is_reply=!(is.na(official_account$reply_to_screen_name))
summary(official_account$is_reply)
sum(official_account$is_reply)/nrow(official_account)

# Analysis of self replies
official_account$t1 = official_account$reply_to_screen_name
official_account$t1[is.na(official_account$reply_to_screen_name)] = ""
official_account$is_self_reply=official_account$t1 == official_account$screen_name
summary(official_account$is_self_reply)
sum(official_account$is_self_reply)/nrow(official_account)
self_reply=official_account[official_account$is_self_reply==1,]

# Analysis on private replies (all replies excluding self-replies)
official_account$is_private = 
  official_account$is_reply == 1&official_account$is_self_reply==0
summary(official_account$is_private)
sum(official_account$is_private)/nrow(official_account)
private=official_account[official_account$is_private==1,]


### Analysis on the effectiveness of official account messages 
# Count of likes & the tweet with most likes
sum(official_account$favorite_count)
summary(official_account$favorite_count)
ct=table(official_account$favorite_count)
par(mar = c(3,3,3,3))
barplot(ct)
hist(official_account$favorite_count, nclass=1000)
favorite_tweet <- official_account %>% slice_max(favorite_count)
favorite_tweet$text
# findings: The most popular tweet is the original tweet with 53217 likes & 10234 retweets

# Which type of messages received more likes
summary(retweet$favorite_count)
summary(quote$favorite_count)
summary(private$favorite_count)
summary(self_reply$favorite_count)
summary(original$favorite_count)

