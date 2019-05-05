library(rtweet)
library(readr)

# search for 18,000 tweets (the maximum) in Engligh
stream <- search_tweets("lang:en", n=18000)

# change the date and time of when the tweets are collected
write_as_csv(stream, "date")
