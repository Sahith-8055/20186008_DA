tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(corpus)

allTweets <- as.data.frame(as.matrix(dtm))
str(allTweets)
ncol(allTweets)
# There are 3780 unique words that are present in all the tweets.

install.packages("wordcloud")
library(wordcloud)
?wordcloud
# We should use colnames(allTweets) to get a vector of words


colSums(allTweets)
rowSums(allTweets)

wordCloud <- wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2, 0.25))
# apple is the most common word across all tweets.

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))

dtm <- DocumentTermMatrix(corpus)

allTweets <- as.data.frame(as.matrix(dtm))

wordCloud <- wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125))

negativeTweets <- subset(allTweets, tweets$Avg <= -1)
colSums(negativeTweets)
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale = c(2, 0.25))

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), random.order = FALSE)

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), rot.per = 0.5)

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), random.order = FALSE, random.color = TRUE)


install.packages("RColorBrewer")
library(RColorBrewer)

?brewer.pal

display.brewer.all()

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), random.order = FALSE, colors = brewer.pal(9, "Blues"))


wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), random.order = FALSE, colors = brewer.pal(9, "Blues")[c(-5,-6,-7,-8,-9)])

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), random.order = FALSE, colors = brewer.pal(9, "Blues")[c(-1,-2,-3,-4)])

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), random.order = FALSE, colors = brewer.pal(9, "Blues")[c(1,2,3,4)])

wordcloud(colnames(allTweets), colSums(allTweets), scale = c(1, 0.125), random.order = FALSE, colors = brewer.pal(9, "Blues")[c(5,6,7,8,9)])
