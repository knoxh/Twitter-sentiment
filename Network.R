source("Clean_tweets.R")

library(rtweet)
library(readr)
library(igraph)
library(dplyr)
library(rgexf)

# insert the index of the positive and negative tweet (index in tweets) 
# selected from sentiment.R
# index <- *insert index value
tweet_index <- pos_network_index[[index]]

# get retweet status id
og_tweet_ids <- tweets[tweet_index,]$retweet_status_id

# function that finds the original tweeter's (author) user id
originalTweeter <- function(status_id){
  originalTweet <- lookup_statuses(status_id)
  originalTweeter <- data.frame(user_id = originalTweet$user_id)
  return(originalTweeter)
}

# call the function above
original_tweeters <- originalTweeter(og_tweet_ids)

# get the retweeters of the tweet
# include the author so we get their friends
retweeters <- function(status_id, original_tweeters) {
    who_retweet <- list()
    who_retweet <- get_retweeters(status_id=status_id)
    who_retweet <- who_retweet[(1:tweets[tweet_index,]$retweet_count),]
    who_retweet <- append(who_retweet, as.character(original_tweeters$user_id))
  return(who_retweet)
}

# call the function above
who_retweet <- retweeters(og_tweet_ids, original_tweeters)


# determine if any of the retweeters have more than 10,000 friends
# if so, will have to get the friends by manually get 10k at a time
lookup_users(who_retweet)$friends_count

# who_retweet index of people with no friends
# won't work in the getFriends function if they have no friends

# single_node <- *insert index value of people with no friends


big_friends <- list()
# Now we fill the files with the friends of each retweeter
# this take a long time
getFriends <- function(who_retweet, directory){
  
  for(i in setdiff(1:length(who_retweet),(single_node))){
    retweeter <- who_retweet[[i]]
    df.friend <- data.frame()
    
    rpt <- lookup_users(retweeter)$friends_count/5000
    
    if(rpt <= 2){
    
    friends <- get_friends(retweeter, n = 5000, retryonratelimit = TRUE)
    friends <- dplyr::select(friends, user_id)
    df.friend <- rbind(df.friend, friends)
    Sys.sleep(60)
    
    if(rpt > 1){
      friend <- friends
      page <- next_cursor(friend)
      friend <- get_friends(retweeter, n = 5000, page = page, retryonratelimit = TRUE)
      friend <- dplyr::select(friend, user_id)
      df.friend <- rbind(df.friend, friend)
      Sys.sleep(60)
    } 
    # create file for each retweeter containing their friends' user ids
    fileName <- paste0(directory, "/userId_", retweeter, ".txt")
    write.table(df.friend, file = fileName, row.names = FALSE) 
    limits <- rate_limit("friends/ids")
    cat("Number of friends for: ",i, " ", retweeter, nrow(df.friend), "  ", " should be ", lookup_users(retweeter)$friends_count,"\n")
    } else{
      big_friends <- append(big_friends, retweeter)
      next
    }
    }
  return(big_friends)
}
retweetersID <- getFriends(who_retweet, getwd())

################################################################
# This is to get network of followers and compare to friends.
# DO NOT need to run this every time!
# It's good to check followers network is same as friends network if the 
# network looks weird.

  edgeList2 <- data.frame()
  for(i in 1:length(who_retweet)){
    retweeter <- (who_retweet[[19]])
    followers <- get_followers(retweeter, n = 5000, retryonratelimit = TRUE)
    tryCatch(common <- intersect(who_retweet, followers$user_id))
    if(length(common)>0){
      e <- unname(cbind(common, as.character(who_retweet[[i]])))
      edgeList2 <- rbind(edgeList2, e)
    }else{next}
  }
 
  # change column names of edgeList to fit Gephi format
  colnames(edgeList2) <- c("source","target")

#################################################################
# edge list function for friends network
createEdgeList2 <- function(who_reweet){
  retweeters <- setdiff(who_retweet, who_retweet[single_node])
  directory <- getwd()
  searchStr <- paste0(directory, "/userId_", retweeters, ".txt")
  files <- Sys.glob(searchStr)
  
  edgeList <- data.frame()
  
  # intersect the files and the list of retweeters
  for (i in 1:length(files)){
    # some of the users are private, so they don't have a file 
    # silent=TRUE allows the code to keep running even if the file doesn't exist
    friends <- try(read_csv(searchStr[i], col_types = cols(user_id = col_character())), silent=TRUE)
    common <- try(intersect(friends$user_id, retweeters), silent=TRUE)
    
    # edge list
    if(length(common) >0) {
      # should be the id in the searchStr element
      e <- unname(cbind(common, retweeters[[i]]))
      edgeList <- rbind(edgeList, e)
    } 
  } 
  colnames(edgeList) <- c("source","target")
  return(edgeList)
}

edgeList <- createEdgeList2(who_retweet)

# builds undirected network, colors the author's vertex blue
graph <- function(edgeList, originalTweeter){
  g <- graph_from_edgelist(edgeList)
  V(g)$color <- "black"
  originalTweeterID <- originalTweeter$user_id
  V(g)[as.character(originalTweeterID)]$color <- "blue"
  graph <- plot(as.undirected(g), vertex.size = 6, vertex.label = NA, edge.width = 1, layout = layout.fruchterman.reingold(g))
  return(graph)
}

# call the graph function above
g <- graph(as.matrix(edgeList), original_tweeters)

# write edge list as file to export into Gephi
write.csv(edgeList2, file = paste0(as.character(original_tweeters$user_id), "_date&time_", "friends_edgelist"))