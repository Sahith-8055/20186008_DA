stocks <- read.csv("StocksCluster.csv")
nrow(stocks)
# There are 11580 observations

tbl1 <- table(stocks$PositiveDec)
tbl1[2] / sum(tbl1[1], tbl1[2])
# The proportion of observations that have positive returns in December is 0.546114.

str(stocks)
cor(stocks)
# Largest coefficient is 0.19167279 between ReturnOct and ReturnNov


summary(stocks)
# ReturnApr and ReturnSep have the highest and lowest averages.

set.seed(144)
library(caTools)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl==TRUE)
stocksTest <- subset(stocks, spl==FALSE)

StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = "binomial")
StocksPredict <- predict(StocksModel, type = "response")
tbl2 <- table(stocksTrain$PositiveDec, StocksPredict >= 0.5)
sum(diag(tbl2)) / nrow(stocksTrain)
# The accuracy of the model is 0.5711818.

StocksTestPredict <- predict(StocksModel, newdata = stocksTest, type = "response")
tbl3 <- table(stocksTest$PositiveDec, StocksTestPredict >= 0.5)
sum(diag(tbl3)) / nrow(stocksTest)
# The accuracy of the model on test set is 0.5670697.

tbl4 <- table(stocksTest$PositiveDec)
tbl4[2] / sum(tbl4[1], tbl4[2])
# The accuracy of the baseline model where PositiveDec==1 is 0.5460564.

limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
# 2.100586e-17
mean(normTest$ReturnJan)
# -0.0004185886

set.seed(144)
km <- kmeans(normTrain, centers = 3)
table(km$cluster)
km$size
# Cluster 2 has largest number of observations (4696)

library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata = normTest)
table(clusterTest)
# There are 2080 observations in Cluster 2.

stocksTrain1 <- subset(stocksTrain, km$cluster == 1)
stocksTrain2 <- subset(stocksTrain, km$cluster == 2)
stocksTrain3 <- subset(stocksTrain, km$cluster == 3)

stocksTest1 <- subset(stocksTest, km$cluster == 1)
stocksTest2 <- subset(stocksTest, km$cluster == 2)
stocksTest3 <- subset(stocksTest, km$cluster == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
# stocksTrain1 data frame has the highest average value of the dependent variable.

StocksModel1 <- glm(PositiveDec ~ ., data = stocksTrain1, family = "binomial")
StocksModel2 <- glm(PositiveDec ~ ., data = stocksTrain2, family = "binomial")
StocksModel3 <- glm(PositiveDec ~ ., data = stocksTrain3, family = "binomial")
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)


predictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = "response")
tbl5 <- table(stocksTest1$PositiveDec, predictTest1 >= 0.5)
tbl5
sum(diag(tbl5)) / nrow(stocksTest1)


predictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = "response")
tbl6 <- table(stocksTest2$PositiveDec, predictTest2 >= 0.5)
sum(diag(tbl6)) / nrow(stocksTest2)


predictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = "response")
tbl7 <- table(stocksTest3$PositiveDec, predictTest3 >= 0.5)
sum(diag(tbl7)) / nrow(stocksTest3)


AllPredictions <- c(predictTest1, predictTest2, predictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

tbl8 <- table(AllOutcomes, AllPredictions >= 0.5)
sum(diag(tbl8)) / (531+1046+498+1399)
