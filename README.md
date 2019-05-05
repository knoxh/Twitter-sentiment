# Twitter-sentiment
In chronologcial order:
1) Get_Tweets.R gets 18,000 random tweets. We use this function twice a day (12pm and 10pm) for two weeks.
2) Clean_tweets.R cleans the text of the tweets so we can calculate sentiment without any weird characters, such as emojis, links, symbols, etc.
   We also remove duplicate tweets, i.e. we do not include all of the retweets of a tweet if we have that tweet already in our data.
3) Sentiment.R calculates the sentiment of the tweets using the sentimentR package. It also determines which tweets we use to build our 
   retweet networks. We select one positive and one negative tweet from each data collection to build a retweet network for.
4) Network.R builds the retweet networks based on the friends of the retweeters.
5) analysis measures.R calculates network centrality measures for the networks.
6) Analysis.R makes important figures, showing how many positive and negative tweets there are each day, comparing sentiment to retweet count, 
   comparing the network centrality measures for the networks, etc.
