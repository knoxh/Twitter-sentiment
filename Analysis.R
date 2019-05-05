# Analysis of all the data
library(rtweet)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
library(gridExtra)
library(igraph)



sat1 <- read_twitter_csv("Oct27_10pm_table_sent.csv")
sun1 <- read_twitter_csv("Oct28_10pm_table_sent.csv")   
mon1 <- read_twitter_csv("Oct29_10pm_table_sent.csv")
tue1 <- read_twitter_csv("Oct30_10pm_table_sent.csv")
wed1 <- read_twitter_csv("Oct31_10pm_table_sent.csv")
thur1 <- read_twitter_csv("Nov1_10pm_table_sent.csv")
fri1 <- read_twitter_csv("Nov2_10pm_table_sent.csv")

sat2 <- read_twitter_csv("Nov3_table_sent.csv")
sun2 <- read_twitter_csv("Nov4_table_sent.csv")
mon2 <- read_twitter_csv("Nov5_table_sent.csv")
tue2 <- read_twitter_csv("Nov6_table_sent.csv")
wed2 <- read_twitter_csv("Nov7_table_sent.csv")
thur2 <- read_twitter_csv("Nov8_table_sent.csv")
fri2 <- read_twitter_csv("Nov9_table_sent.csv")

sun <- rbind(sun1, sun2)
mon <- rbind(mon1, mon2)
tue <- rbind(tue1, tue2)
wed <- rbind(wed1, wed2)
thur <- rbind(thur1, thur2)
fri <- rbind(fri1, fri2)
sat <- rbind(sat1, sat2)

# all of our data - our main data frame for this script
week <- rbind(mutate(sat, category = "Saturday"), 
              mutate(sun, category = "Sunday"),
              mutate(mon, category = "Monday"),
              mutate(tue, category = "Tuesday"),
              mutate(wed, category = "Wednesday"),
              mutate(thur, category = "Thursday"),
              mutate(fri, category = "Friday"))

# removes tweets with sentiment score = 0 (i.e. neutral tweets)
# creates new data frame for this - week.no.zero
ind <- which(week$sentiment == 0)
week.no.zero <- week[-ind,]

# categorizes tweets into positive, neagtive and neutral based 
# on sentiment score
week$pos_neg <- ifelse(week$sentiment > 0, "positive", "negative")
index <- which(week$sentiment == 0)
week[index,]$pos_neg <- "neutral"

# organizes data by correct order of days of the week
levels(week$category) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# histogram of sentiment scores - omitting sentiment score = 0
hist(week.no.zero$sentiment, breaks=500, xlim =range(-1.5:1.5), xlab = "Sentiment score", ylab = "Frequency",
     main = "Distribution of sentiment for two weeks omitting zeros")

# another, nicer boxplot of the same as above
qplot(week.no.zero$sentiment,
      geom="histogram",
      binwidth = 0.1,  
      main = "Histogram for sentiment of data - omitting neutral", 
      xlab = "Sentiment",
      ylab = "Count",
      xlim =range(-1.5:1.5),
      col=I("blue")) + theme_classic()

# cut sentiment into 0.5 chunks
week$sentiment.cut <- cut(week$sentiment, breaks = seq(-2.5,2.5,0.5))

# boxplots of sentiment and retweet count by day
ggplot(data = transform(week,category = factor(category,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))), aes(y = log(retweet_count+1), x = factor(sentiment.cut))) +
  geom_boxplot( outlier.shape = NA,outlier.colour = NA)+
  geom_point(color="black", size = 0.1)+
  labs(x = "Sentiment", y = "log(retweet count + 1)", title= "Retweet count by sentiment for each day" )+
  scale_x_discrete(breaks=seq(-2.5,2.5,0.5)) + facet_wrap(~category) + geom_smooth(se=TRUE, aes(group=1)) + theme_classic()

# boxplots of sentiment - all data
ggplot(week, aes(y = log(retweet_count+1), x = factor(sentiment.cut))) +
  geom_boxplot( outlier.shape = NA,outlier.colour = NA)+
  geom_point(color="black", size = 0.1)+
  labs(x = "Sentiment", y = "log(retweet count + 1)", title= "Retweet count by sentiment" )+
  scale_x_discrete(breaks=seq(-2.5,2.5,0.5)) + geom_smooth(se=TRUE, aes(group=1)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) + theme_classic()

# for each day of the week, calculate the total # of positive
# and negative tweets
c <- data.frame()
  for(j in 1:7){
  p <- sum(week$sentiment > 0 & week$category == levels(week$category)[j])
  n <- sum(week$sentiment < 0 & week$category == levels(week$category)[j])
  c <- rbind(c, c(p, n))
  }
rownames(c) <- levels(week$category)
colnames(c) <- c("Positive", "Negative")

# fix formatting
Day <- levels(week$category)
Positive <- c$Positive
Negative <- c$Negative
dd <- data.frame(Day, Positive, Negative)
dd.long <- gather(dd, posneg, Count, -Day)

# number of positive and negative tweets each day of the week
ggplot(data=dd.long, aes(x=Day, y=Count, fill=posneg)) + 
  geom_col(position=position_dodge()) +
  labs(fill = "Sentiment", title = "Tweets sampled each day") +
  scale_x_discrete(limits=Day) +
  theme_classic() +  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Retweet count by sentiment for each day
week$sentiment.cut <- cut(week$sentiment, breaks = seq(-2.5,2.5,0.5))
ggplot(week, aes(y = log(retweet_count+1), x = factor(sentiment.cut))) +
  geom_boxplot( outlier.shape = NA,outlier.colour = NA)+
  geom_point(color="black", size = 0.1)+
  labs(x = "Sentiment", y = "log(retweet count + 1)", title= "Retweet count by sentiment for each day" )+
  scale_x_discrete(breaks=seq(-2.5,2.5,0.5)) + facet_wrap(~category) + geom_smooth(se=TRUE, aes(group=1)) +theme_classic()

# look at following images in 2x2
# reset after
par(mfrow=c(2,2))

retweeted <- NULL
# want tweets that have been retweeted
retweeted <- ifelse(week$retweet_count >1, "Yes", "No")
week$retweeted <- as.factor(retweeted)

# looks at proportion of positive and negative tweets
# that are retweeted at least once
g1 <- ggplot(data=week, aes(pos_neg)) + 
      geom_bar(aes(fill=retweeted), position = "fill") +
      labs(x = "Sentiment", y="Proportion", fill = "Retweeted", title = "Retweet count > 1 by sentiment") +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +theme_classic()

# same as above but for retweet counts > 100
retweeted <- ifelse(week$retweet_count >100, "Yes", "No")
week$retweeted <- as.factor(retweeted)

g2 <- ggplot(data=week, aes(pos_neg)) + 
      geom_bar(aes(fill=retweeted), position = "fill") +
      labs(x = "Sentiment", y="Proportion", fill = "Retweet count > 100", title = "Retweet count > 100 by sentiment") +
      theme(axis.text.x = element_text(angle=60, hjust=1))

# same as above but fore retweet counts > 1000
retweeted <- ifelse(week$retweet_count >1000, "Yes", "No")
week$retweeted <- as.factor(retweeted)

g3 <- ggplot(data=week, aes(pos_neg)) + 
      geom_bar(aes(fill=retweeted), position = "fill") +
      labs(x = "Sentiment", y="Proportion", fill = "Retweet count > 1000", title = "Retweet count > 1000 by sentiment") +
      theme(axis.text.x = element_text(angle=60, hjust=1)) +theme_classic()
# print the graphs
g1
g2
g3

# table of the number of tweets retweeted based on sentiment
retweeted <- as.matrix(table(week$pos_neg, week$retweeted))
# create contingency table
retweeted <- cbind(Yes = c(9299, 5481, 12093), No = c(52661, 43558, 85945))
retweeted

# look at proportions
prop.table(retweeted, margin = 1)

# p-value from hypothesis test < 0.05
f <- fisher.test(retweeted, simulate.p.value = TRUE)
f$p.value


##########################################################################
# looking at retweet networks of selected tweets
# I kept an excel sheet with all of the network
# measures of each network and the data needed analysis
Networks <- read.csv("***excel sheet where I kept track of network data")

# split networks into positive and negative categories
Networks$pos_neg <- if_else(Networks$Sentiment.score > 0, "positive", "negative")

# create dataframe for positive and negative networks
pos <- filter(Networks, Networks$pos_neg == "positive")
neg <- filter(Networks, Networks$pos_neg == "negative")

# data frame of all the network measures I want to compare
posdf <- data.frame(pos$Sentiment.score, pos$group.betweenness, pos$components,pos$group.pagerank, pos$group.degree, pos$Modularity.with.res.1,  pos$group.closeness, pos$communities, pos$Density, pos$Avg..cluster.coef)

# using Spearman correlation to determine correlations 
# among the positive tweets and network measures
Pearson_correlation_matrix <- cor(posdf, method = "spearman")
Pearson_correlation_matrix <- Pearson_correlation_matrix[-c(1,3,4),]

# split grid into 2x3
par(mfrow = c(2,3))

# group betweenness difference between pos. & neg. networks
g1 <- qplot(pos_neg, group.betweenness,data = Networks, geom = "boxplot", aes(fill = pos_neg)) + 
  labs(x= "Sentiment", y = "Group betweenness", title = "Betweenness")+
  guides(fill=FALSE) +theme_classic()

# same as above for modularity
g2 <- qplot(pos_neg, Modularity.with.res.1, data = Networks, geom = "boxplot", aes(fill = pos_neg))+ 
  labs(x= "Sentiment", y = "Modularity", title = "Modularity")+
  guides(fill=FALSE)+theme_classic()

# same as above for number of communities
g3 <- qplot(pos_neg, communities, data = Networks, geom = "boxplot", aes(fill = pos_neg))+ 
  labs(x= "Sentiment", y = "Number of communities", title = "Communities") +
  guides(fill=FALSE) +theme_classic()

# same as above for group degree centrality
g4 <- qplot(pos_neg, group.degree, data = Networks, geom = "boxplot", aes(fill = pos_neg))+ 
  labs(x= "Sentiment", y = "Group degree", title = "Degree") +
  guides(fill=FALSE)+theme_classic()

# same as above for avg. clustering coefficient
g5 <- qplot(pos_neg, Avg..cluster.coef, data = Networks, geom = "boxplot", aes(fill = pos_neg))+ 
  labs(x= "Sentiment", y = "Avg. clustering coeff.", title = "Avg. clustering coefficient") +
  guides(fill=FALSE)+theme_classic()

# same as above for group closeness
g6 <- qplot(pos_neg, group.closeness, data = Networks, geom = "boxplot", aes(fill = pos_neg))+ 
  labs(x= "Sentiment", y = " Group closeness", title = "Closeness") +
  guides(fill=FALSE)+theme_classic()

grid.arrange(g1,g2,g3,g4,g5,g6, nrow=3, ncol=2)

# now look at negative correlations
negdf <- data.frame(neg$Sentiment.score, neg$group.betweenness, neg$components,neg$group.pagerank, neg$group.degree, neg$group.closeness, neg$Modularity.with.res.1, neg$communities, neg$Density, neg$Avg..cluster.coef)
Pearson_correlation_matrix2 <- cor(negdf, method = "spearman")
Pearson_correlation_matrix2 <- Pearson_correlation_matrix2[-c(1,3,4),]

# the correlation values for positive and negative networks, respectively
pos.pvalue <- c(0.4821,0.883,0.5884,0.5894,0.4273,0.6807,0.3375)
neg.pvalue <- c(0.761,0.1846,0.01186,0.1865,0.1254,0.2721,0.1401)

cor.table <- cbind(Pearson_correlation_matrix[,1], pos.pvalue)
cor.table <- cbind(cor.table, Pearson_correlation_matrix2[,1])
cor.table <- cbind(cor.table, neg.pvalue)
cor.table 

# table of positive and negative network correlations
colnames(cor.table) <- c("r(Positive Sentiment)", "r(Negative Sentiment)")
cor.table

require(cocor)

# use cocor to compare the correlations
corrr <- function(pos, neg){
c <- cocor.indep.groups(r1.jk=pos, r2.hm=neg, n1=18, n2=17, alternative="two.sided", 
                   alpha=0.05, conf.level=0.95, null.value=0)
return(c)
}  
for(i in 1:nrow(cor.table)){
 print(corrr(cor.table[i,1], cor.table[i,3]))
}
Difference <- c(0.0973, -0.3006,-0.1998, 0.458, 0.5861, -0.3866,-0.6132)
Pvalue <-     c(0.7898,  0.3976, 0.5672,0.1409, 0.1007, 0.288,0.0866)
cor.table <- cbind(cor.table, Difference)
cor.table <- cbind(cor.table, Pvalue)
rownames(cor.table) <- c("Group Betweenness", "Group Degree", "Modularity", 
                         "Group Closeness", "Communities", "Density", "Avg. Clustering Coefficient")
colnames(cor.table) <- c("r(positive)", "p-value (pos)","r(negative)", "p-value (neg)", "Difference in correlation", "p-value (difference)")
View(cor.table)
write.csv(cor.table, "Table")

library(hexbin)
x <- rnorm(1000)
y <- rnorm(1000)
bin<-hexbin(Networks$Sentiment.score, Networks$communities, xbins=50) 
plot(bin, main="Hexagonal Binning")

# Number of communities based on sentiment
ggplot(Networks, aes(y = communities, x = Sentiment.score, colour = pos_neg)) +
  geom_point()+
  labs(x = "Sentiment", y = "Number of communities", colour = "Sentiment",title = "Number of communities based on sentiment" )+ 
  geom_smooth(span = 1.8, se = FALSE)
