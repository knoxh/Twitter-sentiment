library(readr)
library(rtweet)
library(dplyr)
library(rgexf)
library(tidytext)
library(stringr)
library(sentimentr)
library(tm)
library(knitr)

# 1. Read the data file from Get_Tweets.R
read <- read_twitter_csv("Oct27-12pm.csv")

# 2. Clean the text of the tweets

# rename the tweets
tweets <- read


# remove punctuation, emojis, links, unnecessary symbols
cleaning <- function(text){
  clean_tweet <- gsub("http.*","",  text)
  clean_tweet <- gsub("https.*","", clean_tweet)
  clean_tweet = gsub("&amp", "", clean_tweet)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
  clean_tweet = gsub("@\\w+", "", clean_tweet)
  clean_tweet = gsub("http\\w+", "", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  clean_tweet = gsub("<[^\\>]*>", "", clean_tweet)
  clean_tweet = gsub("<>", "", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
  clean_tweet = gsub("\n", "", clean_tweet)
  clean_tweet = gsub("RT", "", clean_tweet)
return(clean_tweet)
}

# add cleaned text as a column of the tweets dataframe
tweets$clean_text <- cleaning(tweets$text)

# make sure the cleaned tweets are characters
tweets$clean_text <- as.character(tweets$clean_text)

# remove tweets that have no words after being cleaned -- not important for analysis
index <- which(tweets$clean_text == "")
tweets <- tweets[-index,]

# get unique tweets 
# we want the row numbers for duplicates so we can remove
# the entire row of data for duplicate tweets
unique <- unique(tweets$clean_text)
un <- vector()
l <- list()
for(i in 1:nrow(tweets)){
  if(tweets[i,]$clean_text %in% unique){
    if(!tweets[i,]$clean_text %in% un){
      un <- append(un, tweets[i,]$clean_text)
    }
    else{
      l <- append(l, i)
    }
  }
}

# remove rows of duplicates
l <- unlist(l)
tweets <- tweets[-l,]

# save in case I mess it up
# change the date based on data set
write_as_csv(tweets, "tweet_table_date")
