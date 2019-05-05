source("Clean_tweets.R")
source("Get_tweets.R")

library(rtweet)
library(dplyr)
library(rgexf)
library(tidytext)
library(tidyverse)  
library(stringr)
library(sentimentr)
library(tm)

# get sentiment

# initialize dataframe
sent <- data.frame()

# for each tweets, calcualte sentiment and add it to 
# the next row in sent dataframe - takes about an hour for 
# 18,000 tweets
for(i in 1:nrow(tweets)){
  s <- sentiment(tweets[i,]$clean_text)
  sent <- rbind(sent, sum(s$sentiment))
}

# round sentiment score to 6 characters, i.e. 4 decimal values
tweets$sentiment <- round(sent$X0, 6)

# rearrange the rows of tweets by descending sentiment score
tweets <- arrange(tweets, desc(sentiment))

# save the sentiment
# change the date and time based on collection date
write_as_csv(tweets, "date&time_table_sent")

# Choosing tweets for building 'retweet' networks

# get indices of highly positive or highly negative sentiment tweets
pos_index <- which(tweets$sentiment > 0.5)
neg_index <- which(tweets$sentiment < -0.5)

# get tweets with retweet count between 50 and 100
index <- which(tweets$retweet_count < 100 & tweets$retweet_count >= 50)

# get tweets that are between 50-100 retweets and are 
# highly positive or negative in sentiment
pos_network_index <- intersect(index, pos_index)
neg_network_index <- intersect(index, neg_index)

# look at the sentiment values for the above tweets
tweets[pos_network_index,]$sentiment
tweets[neg_network_index,]$sentiment

# choose a positive and a negative tweet from pos_network_index
# and neg_network_index that are close in abs(sentiment score)
# and have a similar number of retweets so we can compare them

# initialize lists
m <- list()
comp <- list()

# function to compare the highly positive and highly negative tweets
choose_networks <- function(pos_network_index, neg_network_index, tweets2)
for(i in 1:1){
  m <- append(m, lapply(neg_network_index, function(x) abs(abs(tweets2[x,]$sentiment) - abs(tweets2[pos_network_index[[1]],]$sentiment))))
  
  cbind(pos_network_index[[i]], m==min(m) )
  comp <- append(comp, min(m))
  
}


# most positive tweets with tweet id and retweet count
positive_ids <- which(!is.na(tweets[1:10,]$retweet_status_id))
positive_retweet_id <- tweets[positive_ids,]$retweet_status_id
positive_retweet_count <- lookup_statuses(positive_retweet_id)$retweet_count
rbind(positive_retweet_id, positive_retweet_count)

# most negative tweets with tweet id and retweet count
negative_ids <- which(!is.na(tweets[nrow(tweets)-10:nrow(tweets),]$retweet_status_id))
negative_retweet_id <- tweets[(nrow(tweets)-10):nrow(tweets),]$retweet_status_id
negative_retweet_count <- lookup_statuses(negative_retweet_id)$retweet_count
rbind(negative_retweet_id, negative_retweet_count)


# omit tweets with a sentiment score = 0
# create new dataframe of nonzero sentiment tweets, nonzero
nonzero <- filter(tweets, sentiment != 0)
nonzero$pos_neg <- if_else(nonzero$sentiment > 0, "positive", "negative")

s <- split(nonzero$sentiment, nonzero$pos_neg)

# compare the distributions of the positive and negative tweets' sentiment
summary(abs(s$negative))
summary(s$positive)

# proportion of positive and negative tweets
sum(nonzero$pos_neg == "positive") / nrow(nonzero)
sum(nonzero$pos_neg == "negative") / nrow(nonzero)

positive <- filter(tweets, sentiment > 0)
negative <- filter(tweets, sentiment < 0)

# boxplot of positive vs negative sentiments
boxplot(positive$sentiment, negative$sentiment, xlab = c("Positive", "Negative"))

require(scales)
ggplot(nonzero) + geom_boxplot(aes(x = pos_neg, y = retweet_count, fill = pos_neg)) +
  theme(legend.position = "none") + scale_y_continuous(labels = comma) +
  labs(x = "Positive or Negative Sentiment", y = "Retweet count", title = "Retweet Count by Sentiment- date&time of data") 

# remove outliers, i.e. retweet counts > 25000
scatterplot_data <- filter(tweets, retweet_count < 25000)

# scatterplot of number of retweets vs sentiment score
scatterplot <- ggplot(data = scatterplot_data, aes(sentiment, retweet_count)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "Sentiment score", y = "Retweet count", title = "Sentiment score and retweet count - Oct 22, 6pm") 

scatterplot + scale_y_continuous(labels = comma)

# save the dataframe with sentiment scores 
write_as_csv(tweets, "date&time_table_sent")
