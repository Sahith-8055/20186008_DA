tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus <- tm_map(corpus, tolower)
corpus[[1]]
corpus <- tm_map(corpus, removePunctuation)
stopwords("english")[1:5]
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]

frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)
sparse <- removeSparseTerms(frequencies, 0.995)
sparse
tweetSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetSparse) <- make.names(colnames(tweetSparse))
tweetSparse$Negative <- tweets$Negative
library(caTools)
set.seed(123)
split <- sample.split(tweetSparse$Negative, SplitRatio = 0.7)
Train <- subset(tweetSparse, split==TRUE)
Test <- subset(tweetSparse, split==FALSE)
library(rpart)
library(rpart.plot)

tweetCART <- rpart(Negative ~ ., data = Train, method = "class")
prp(tweetCART)

predictCART <- predict(tweetCART, newdata = Test, type = "class")
tbl1 <- table(Test$Negative, predictCART)
sum(diag(tbl1)) / nrow(Test)
# Accuracy of the CART model is 0.8788732.

table(Test$Negative)
# Accuracy of baseline model that always predict non-negative sentiments is
300/355
# 0.8450704

library(randomForest)
set.seed(123)
tweetRF <- randomForest(Negative ~ ., data = Train)
predictRF <- predict(tweetRF, newdata = Test)
tbl2 <- table(Test$Negative, predictRF)
sum(diag(tbl2)) / nrow(Test)
# Accuracy of the random forest is 0.8873239.

